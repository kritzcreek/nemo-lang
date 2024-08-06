pub mod vfs;

use frontend::highlight::HighlightKind;
use frontend::ir::Ctx;
use frontend::CheckError;
use line_index::{LineCol, LineIndex, TextRange};
use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::notification::{DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument};
use lsp_types::request::{
    DocumentDiagnosticRequest, GotoDefinition, HoverRequest, SemanticTokensFullRequest,
};
use lsp_types::{
    Diagnostic, DiagnosticOptions, DiagnosticServerCapabilities, DiagnosticSeverity,
    DocumentDiagnosticReport, FullDocumentDiagnosticReport, GotoDefinitionResponse,
    InitializeParams, Location, OneOf, Position, Range, RelatedFullDocumentDiagnosticReport,
    SemanticToken, SemanticTokenType, SemanticTokens, SemanticTokensFullOptions,
    SemanticTokensLegend, SemanticTokensOptions, SemanticTokensServerCapabilities,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, WorkDoneProgressOptions,
};
use serde_json::json;
use std::error::Error;
use vfs::FileData;

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
        diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
            DiagnosticOptions::default(),
        )),
        definition_provider: Some(OneOf::Left(true)),
        hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
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
                let req = match cast_request::<SemanticTokensFullRequest>(req) {
                    Ok((id, params)) => {
                        let uri = params.text_document.uri;
                        let file_path = uri.to_file_path().unwrap();
                        eprintln!("Attempting to read {uri} as file {}", file_path.display());

                        let program = match vfs.read_file(&file_path) {
                            Some(program) => program,
                            None => {
                                vfs.open_file(file_path.clone()).unwrap();
                                vfs.read_file(&file_path).unwrap()
                            }
                        };

                        let result = SemanticTokens {
                            result_id: None,
                            data: semantic_tokens_new(program),
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
                let req = match cast_request::<DocumentDiagnosticRequest>(req) {
                    Ok((id, params)) => {
                        let uri = params.text_document.uri;
                        let file_path = uri.to_file_path().unwrap();
                        eprintln!("Attempting to read {uri} as file {}", file_path.display());

                        let file_data = match vfs.read_file(&file_path) {
                            Some(program) => program,
                            None => {
                                vfs.open_file(file_path.clone()).unwrap();
                                vfs.read_file(&file_path).unwrap()
                            }
                        };
                        let result =
                            DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                                related_documents: None,
                                full_document_diagnostic_report: FullDocumentDiagnosticReport {
                                    result_id: None,
                                    items: file_data
                                        .check_result
                                        .errors
                                        .iter()
                                        .map(|e| {
                                            make_diagnostic(
                                                e,
                                                &file_data.check_result.names,
                                                &file_data.line_index,
                                            )
                                        })
                                        .collect(),
                                },
                            });
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
                let req = match cast_request::<GotoDefinition>(req) {
                    Ok((id, params)) => {
                        let uri = params.text_document_position_params.text_document.uri;
                        let position = params.text_document_position_params.position;
                        let file_path = uri.to_file_path().unwrap();
                        eprintln!("Attempting to read {uri} as file {}", file_path.display());

                        let file_data = match vfs.read_file(&file_path) {
                            Some(program) => program,
                            None => {
                                vfs.open_file(file_path.clone()).unwrap();
                                vfs.read_file(&file_path).unwrap()
                            }
                        };
                        let Some(range) = find_definition(file_data, &position) else {
                            connection.sender.send(Message::Response(Response {
                                id,
                                result: Some(json!("null")),
                                error: None,
                            }))?;
                            continue;
                        };
                        let result = GotoDefinitionResponse::Scalar(Location { uri, range });
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
                let _req = match cast_request::<HoverRequest>(req) {
                    Ok((_id, _params)) => {
                        // let uri = params.text_document_position_params.text_document.uri;
                        // let position = params.text_document_position_params.position;
                        // let file_path = uri.to_file_path().unwrap();
                        // eprintln!("Attempting to read {uri} as file {}", file_path.display());

                        // let file_data = match vfs.read_file(&file_path) {
                        //     Some(program) => program,
                        //     None => {
                        //         vfs.open_file(file_path.clone()).unwrap();
                        //         vfs.read_file(&file_path).unwrap()
                        //     }
                        // };
                        // let Some(hover_response) = hover(file_data, &position) else {
                        //     connection.sender.send(Message::Response(Response {
                        //         id,
                        //         result: Some(json!("null")),
                        //         error: None,
                        //     }))?;
                        //     continue;
                        // };
                        //let result = serde_json::to_value(&hover_response).unwrap();

                        //let resp = Response {
                        //    id,
                        //    result: Some(result),
                        //    error: None,
                        //};
                        //connection.sender.send(Message::Response(resp))?;
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
                            &params.text_document.uri.to_file_path().unwrap(),
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

fn make_diagnostic(error: &CheckError, ctx: &Ctx, line_index: &LineIndex) -> Diagnostic {
    let (start, end) = error.line_col(line_index);
    Diagnostic {
        range: Range {
            start: Position {
                line: start.line,
                character: start.col,
            },
            end: Position {
                line: end.line,
                character: end.col,
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: Some("nemo".to_string()),
        message: error.message(ctx),
        related_information: None,
        tags: None,
        data: None,
    }
}

pub const HIGHLIGHT_NAMES: [&str; 7] = [
    "keyword", "type", "function", "operator", "property", "number", "comment",
];

fn semantic_tokens_new(file_data: &FileData) -> Vec<SemanticToken> {
    let hls = frontend::highlight::highlight(
        &file_data.check_result.parse,
        &file_data.check_result.occurrences,
    );
    let mut tokens = vec![];
    let mut prev_token_line_col: LineCol = LineCol { line: 0, col: 0 };

    for hl in hls {
        let line_col = file_data.line_index.line_col(hl.range.start());
        let delta_line = line_col.line - prev_token_line_col.line;
        let delta_start = if delta_line == 0 {
            line_col.col - prev_token_line_col.col
        } else {
            line_col.col
        };
        let length = hl.range.len().into();
        let token_type = match hl.kind {
            HighlightKind::Keyword => 0,
            HighlightKind::Type => 1,
            HighlightKind::Function => 2,
            HighlightKind::Operator => 3,
            HighlightKind::Property => 4,
            HighlightKind::Literal => 5,
            HighlightKind::Comment => 6,
            HighlightKind::Local | HighlightKind::Global => continue,
        };
        tokens.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type,
            token_modifiers_bitset: 0,
        });
        prev_token_line_col = line_col;
    }

    tokens
}

fn resolve_text_range(range: &TextRange, line_index: &LineIndex) -> Option<Range> {
    let start = line_index.try_line_col(range.start())?;
    let end = line_index.try_line_col(range.end())?;
    Some(Range {
        start: Position {
            line: start.line,
            character: start.col,
        },
        end: Position {
            line: end.line,
            character: end.col,
        },
    })
}

fn find_definition(file_data: &FileData, position: &Position) -> Option<Range> {
    let offset = file_data.line_index.offset(LineCol {
        line: position.line,
        col: position.character,
    })?;
    let (_, occurrence) = file_data
        .check_result
        .occurrences
        .iter()
        .find(|(node_ptr, _)| node_ptr.0.contains(offset))?;
    let name = occurrence.name();
    let range = file_data.check_result.names.resolve(*name).1;
    resolve_text_range(&range, &file_data.line_index)
}

// fn hover(file_data: &FileData, position: &Position) -> Option<Hover> {
//     let offset = file_data.line_index.offset(LineCol {
//         line: position.line,
//         col: position.character,
//     })?;
//
//     let mut matches = vec![];
//     for (ptr, ty) in &file_data.check_result.typed_nodes {
//         if ptr.text_range().contains(offset) {
//             matches.push((ptr, ty));
//         }
//     }
//
//     if matches.is_empty() {
//         return None;
//     }
//     let mut matches = matches.into_iter();
//     let mut narrowest_match = matches.next().unwrap();
//     for (ptr, ty) in matches {
//         if ptr.text_range().len() < narrowest_match.0.text_range().len() {
//             narrowest_match = (ptr, ty);
//         }
//     }
//
//     Some(Hover {
//         contents: HoverContents::Markup(MarkupContent {
//             kind: MarkupKind::Markdown,
//             value: format!(
//                 "{}",
//                 narrowest_match
//                     .1
//                     .display(&file_data.check_result.names.name_map)
//             ),
//         }),
//         range: resolve_text_range(&narrowest_match.0.text_range(), &file_data.line_index),
//     })
// }

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
