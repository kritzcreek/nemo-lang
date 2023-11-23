pub mod vfs;

use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::notification::{DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument};
use lsp_types::request::SemanticTokensFullRequest;
use lsp_types::{
    InitializeParams, SemanticToken, SemanticTokenType, SemanticTokens, SemanticTokensFullOptions,
    SemanticTokensLegend, SemanticTokensOptions, SemanticTokensServerCapabilities,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, WorkDoneProgressOptions,
};
use nemo_frontend::parser::HIGHLIGHT_NAMES;
use std::error::Error;
use tree_sitter_highlight::{Highlight, HighlightEvent};

use crate::vfs::Vfs;

pub fn start_language_server() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Note that  we must have our logging only write out to stderr.
    eprintln!("starting generic LSP server");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
                legend: SemanticTokensLegend {
                    token_types: HIGHLIGHT_NAMES
                        .into_iter()
                        .map(SemanticTokenType::new)
                        .collect(),
                    token_modifiers: vec![],
                },
                range: None,
                full: Some(SemanticTokensFullOptions::Bool(true)),
            },
        )),
        ..Default::default()
    })
    .unwrap();
    let initialization_params = connection.initialize(server_capabilities)?;
    eprintln!("{}", initialization_params);
    main_loop(connection, initialization_params)?;
    io_threads.join()?;

    // Shut down gracefully.
    eprintln!("shutting down server");
    Ok(())
}

fn main_loop(
    connection: Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let mut vfs = Vfs::new();

    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    eprintln!("starting example main loop");
    for msg in &connection.receiver {
        eprintln!("got msg: {msg:?}");
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("got request: {req:?}");
                match cast_request::<SemanticTokensFullRequest>(req) {
                    Ok((id, params)) => {
                        let uri = params.text_document.uri;
                        let file_path = uri.to_file_path().unwrap();
                        eprintln!("Attempting to read {uri} as file {}", file_path.display());

                        let program = match vfs.read_file(&file_path) {
                            Some(program) => program.as_str(),
                            None => {
                                vfs.open_file(file_path.clone()).unwrap();
                                vfs.read_file(&file_path).unwrap()
                            }
                        };

                        let events = nemo_frontend::parser::highlight(program);
                        let result = SemanticTokens {
                            result_id: None,
                            data: semantic_tokens(program, events),
                        };
                        let result = serde_json::to_value(&result).unwrap();

                        let resp = Response {
                            id,
                            result: Some(result),
                            error: None,
                        };
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
            }
            Message::Response(resp) => {
                eprintln!("got response: {resp:?}");
            }
            Message::Notification(not) => {
                eprintln!("got notification: {not:?}");
                let not = match cast_notification::<DidOpenTextDocument>(not) {
                    Ok(params) => {
                        vfs.insert_file(
                            params.text_document.uri.to_file_path().unwrap(),
                            params.text_document.text,
                        );
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(not)) => not,
                };
                let not = match cast_notification::<DidChangeTextDocument>(not) {
                    Ok(params) => {
                        vfs.update_file(
                            &params.text_document.uri.to_file_path().unwrap(),
                            params.content_changes[0].text.clone(),
                        );
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(not)) => not,
                };
                let not = match cast_notification::<DidCloseTextDocument>(not) {
                    Ok(params) => {
                        vfs.remove_file(&params.text_document.uri.to_file_path().unwrap());
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(not)) => not,
                };
                eprintln!("got notification: {not:?}");
            }
        }
    }
    Ok(())
}

fn semantic_tokens(content: &str, events: Vec<HighlightEvent>) -> Vec<SemanticToken> {
    let mut tokens = vec![];
    let mut highlight: Option<Highlight> = None;
    let mut prev_token_start = 0;

    for event in events {
        match event {
            HighlightEvent::Source { start, end } => {
                // As long as no highlight type is set we 'concat' all
                // event sources
                let token_type = match highlight {
                    None => continue,
                    Some(Highlight(token_type)) => token_type as u32,
                };

                let skipped = &content[prev_token_start..start];
                let mut delta_line = 0;
                let mut delta_start = 0;
                for line in skipped.split('\n') {
                    delta_line += 1;
                    delta_start = line.chars().count() as u32;
                }
                delta_line -= 1;

                let length = content[start..end].chars().count() as u32;
                tokens.push(SemanticToken {
                    delta_line,
                    delta_start,
                    length,
                    token_type,
                    token_modifiers_bitset: 0,
                });
                prev_token_start = start;
            }
            HighlightEvent::HighlightStart(h) => highlight = Some(h),
            HighlightEvent::HighlightEnd => highlight = None,
        }
    }

    tokens
}

fn cast_request<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

fn cast_notification<R>(not: Notification) -> Result<R::Params, ExtractError<Notification>>
where
    R: lsp_types::notification::Notification,
    R::Params: serde::de::DeserializeOwned,
{
    not.extract(R::METHOD)
}
