# VSCode extension for Nemo

## Functionality

This extension connects to the nemo language server and contributes the nemo language definition for VSCode.

## Installing the extension

TODO: Once the extension isn't changing much, just commit the `.vsix` bundle to speed up installation

- `npm install`
- `npx vsce package`
- `code --install-extension nemo-vscode-1.0.0.vsix`

## Developing the extension

- Run `npm install`
- Open VS Code on this folder.
- Switch to the Run and Debug View in the Sidebar (Ctrl+Shift+D).
- Select `Launch Client` from the drop down (if it is not already).
- Press ▷ to run the launch config (F5).
- Open a `*.nemo` file. You should see semantic syntax highlighting
