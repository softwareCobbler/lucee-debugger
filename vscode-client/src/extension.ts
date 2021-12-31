import * as vscode from "vscode";

// replaced by esbuild at build
declare const DAP_SERVER_JAR_PATH : string;

let currentDebugSession : vscode.DebugSession | null = null;

class CfDebugAdapter implements vscode.DebugAdapterDescriptorFactory {
	createDebugAdapterDescriptor(session: vscode.DebugSession, _executable: vscode.DebugAdapterExecutable | undefined): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {
		const debugdebug = session.configuration.debugdebug ?? null;
		const args : string[] = [];

		if (debugdebug) {
			const what = /^(listen|suspend):(\d+)$/.exec(debugdebug);
			if (what) {
				const suspend = what[1] === "suspend" ? "y" : "n";
				const port = what[2];
				args.push(`-agentlib:jdwp=transport=dt_socket,address=localhost:${port},suspend=${suspend},server=y`)
			}
		}

		args.push("-jar", DAP_SERVER_JAR_PATH);

		currentDebugSession = session;
		return new vscode.DebugAdapterExecutable("java", args);
	}
	
}

export function activate(context: vscode.ExtensionContext) {
	const outputChannel = vscode.window.createOutputChannel("lucee-debugger");
	context.subscriptions.push(outputChannel);
	context.subscriptions.push(vscode.debug.registerDebugAdapterDescriptorFactory("cfml", new CfDebugAdapter()));

	context.subscriptions.push(
		vscode.commands.registerCommand("luceeDebugger.showLoadedClasses", () => {
			currentDebugSession?.customRequest("showLoadedClasses");
		}));

	vscode.debug.registerDebugAdapterTrackerFactory("cfml", {
		createDebugAdapterTracker(session: vscode.DebugSession) {
			return {
				onWillReceiveMessage(message: any) : void {
					outputChannel.append(JSON.stringify(message, null, 4) + "\n");
				},
				onDidSendMessage(message: any) : void {
					outputChannel.append(JSON.stringify(message, null, 4) + "\n");
				}
			}
		}
	})
}

export function deactivate() {
	currentDebugSession = null;
}