use std::error::Error;
use std::fs;

use lsp_types::request::{HoverRequest, SemanticTokensFullRequest};
use lsp_types::{
    request::GotoDefinition, GotoDefinitionResponse, InitializeParams, ServerCapabilities,
};
use lsp_types::{
    Hover, HoverContents, OneOf, SemanticToken, SemanticTokenType, SemanticTokens,
    SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
    SemanticTokensServerCapabilities, WorkDoneProgressOptions,
};

use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};
use tree_sitter_highlight::{Highlight, HighlightEvent};

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Note that  we must have our logging only write out to stderr.
    eprintln!("starting generic LSP server");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        definition_provider: Some(OneOf::Left(true)),
        hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
                legend: SemanticTokensLegend {
                    token_types: vec![
                        SemanticTokenType::KEYWORD,
                        SemanticTokenType::TYPE,
                        SemanticTokenType::FUNCTION,
                        SemanticTokenType::OPERATOR,
                        SemanticTokenType::PROPERTY,
                        SemanticTokenType::NUMBER,
                        SemanticTokenType::COMMENT,
                    ],
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
                let req = match cast::<GotoDefinition>(req) {
                    Ok((id, params)) => {
                        eprintln!("got gotoDefinition request #{id}: {params:?}");
                        let result = Some(GotoDefinitionResponse::Array(Vec::new()));
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

                let req = match cast::<HoverRequest>(req) {
                    Ok((id, _params)) => {
                        let result = Hover {
                            contents: HoverContents::Markup(lsp_types::MarkupContent {
                                kind: lsp_types::MarkupKind::Markdown,
                                value: "Hello hover".to_string(),
                            }),
                            range: None,
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
                match cast::<SemanticTokensFullRequest>(req) {
                    Ok((id, params)) => {
                        let uri = params.text_document.uri;
                        let file_path = uri.to_file_path().unwrap();
                        eprintln!("Attempting to read {uri} as file {}", file_path.display());
                        let program = fs::read_to_string(file_path).unwrap();

                        let events = nemo_frontend::parser::highlight(&program);
                        let result = SemanticTokens {
                            result_id: None,
                            data: semantic_tokens(&program, events),
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
                let token_type = match highlight {
                    None => continue,
                    Some(Highlight(token_type)) => token_type as u32,
                };

                let skipped = &content[prev_token_start..start];
                eprintln!("skipped: {skipped}, {prev_token_start}:{start}");
                let mut delta_line = 0;
                let mut delta_start = 0;
                for line in skipped.split("\n") {
                    delta_line += 1;
                    delta_start = line.chars().count() as u32;
                }
                delta_line -= 1;
                let token_str = &content[start..end];
                eprintln!("token: {token_str}, {start}:{end}");
                let length = token_str.chars().count() as u32;
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

    eprintln!("semantic tokens: {tokens:#?}");
    tokens
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}
