use zed_extension_api as zed;

struct Reality;

impl Reality {
    fn language_server_binary_path(
        &self,
        _language_server_id: &zed_extension_api::LanguageServerId,
        _worktree: &zed_extension_api::Worktree,
    ) -> zed_extension_api::Result<String> {
        Ok("".to_string())
    }
}

impl zed::Extension for Reality {
    fn new() -> Self {
        Reality {}
    }

    fn language_server_command(
        &mut self,
        language_server_id: &zed_extension_api::LanguageServerId,
        worktree: &zed_extension_api::Worktree,
    ) -> zed_extension_api::Result<zed_extension_api::Command> {
        Ok(zed::Command {
            command: self.language_server_binary_path(language_server_id, worktree)?,
            args: vec![],
            env: worktree.shell_env(),
        })
    }
}

zed::register_extension!(Reality);
