import * as vscode from "vscode";

class CfDebugAdapter implements vscode.DebugAdapterDescriptorFactory {
	createDebugAdapterDescriptor(session: vscode.DebugSession, _executable: vscode.DebugAdapterExecutable | undefined): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {
		const debugdebug = session.configuration.debugdebug ?? null;
		const args : string[] = [];

		if (debugdebug) {
			const what = /^(listen|suspend):(\d+)$/.exec(debugdebug);
			if (what) {
				// uh does this always suspend somehow?
				const suspend = what[1] === "suspend" ? "y" : "n";
				const port = what[2];
				args.push(`-agentlib:jdwp=transport=dt_socket,address=localhost:${port},suspend=${suspend},server=y`)
			}
		}

		args.push(
			"-jar",
			"/home/anon/cflsd/dap-server/target/scala-3.1.0/foo-assembly-0.1.0-SNAPSHOT.jar");

		return new vscode.DebugAdapterExecutable("java", args);
	}
}

export function activate(context: vscode.ExtensionContext) {
	context.subscriptions.push(vscode.debug.registerDebugAdapterDescriptorFactory("cfml", new CfDebugAdapter()));
}

export function deactivate() {

}