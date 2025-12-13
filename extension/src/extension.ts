import * as path from "path";
import * as fs from "fs";
import * as vscode from "vscode";
import { LanguageClient, LanguageClientOptions, TransportKind, Executable } from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext) {
  const serverExecutable = process.platform === "win32" ? "lsp.exe" : "lsp";
  const serverPath = path.join(context.extensionPath, "out", serverExecutable);
  console.log(`Glue LSP resolved path: ${serverPath}`);
  if (!fs.existsSync(serverPath)) {
    vscode.window.showErrorMessage("Glue LSP binary not found. Build with 'cargo build -p lsp --release'.");
    return;
  }

  const run: Executable = {
    command: serverPath,
    transport: TransportKind.stdio,
    options: { env: { ...process.env, RUST_LOG: "debug" } },
  };
  const serverOptions = { run, debug: run };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "glue" }],
    synchronize: { fileEvents: vscode.workspace.createFileSystemWatcher("**/*.glue") },
  };

  client = new LanguageClient("glueLsp", "Glue Language Server", serverOptions, clientOptions);
  const disposable = {
    dispose: () => {
      client?.stop();
    },
  };
  client
    .start()
    .then(() => {
      console.log("Glue LSP started");
      context.subscriptions.push(disposable);
    })
    .catch((err) => {
      vscode.window.showErrorMessage(`Failed to start Glue LSP: ${err}`);
    });

  const restartCommand = vscode.commands.registerCommand("glue.restart", async () => {
    if (client) {
      await client.stop();
      await client.start();
      vscode.window.showInformationMessage("Glue LSP restarted");
    }
  });

  const showLogsCommand = vscode.commands.registerCommand("glue.showLogs", async () => {
    if (client) {
      client.outputChannel.show();
    }
  });

  context.subscriptions.push(restartCommand, showLogsCommand);
}

export function deactivate(): Thenable<void> | undefined {
  if (client) {
    return client.stop();
  }
  return undefined;
}
