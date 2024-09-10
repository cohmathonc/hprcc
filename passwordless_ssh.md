# 🔐 Passwordless SSH Setup 

This guide will help you set up passwordless SSH access from your local machine (Mac, Windows, or Linux) to Apollo (`ppxhpcacc01.coh.org`); same will work for Gemini (`gemini-login1.coh.org`).

## 🖥️ Local Machine Setup (Mac, Windows, Linux)

1. Generate an SSH key pair (if you don't already have one):
   ```bash
   ssh-keygen -t ed25519 -C "your_email@coh.org"
   ```
   Press Enter to accept the default file location and enter a passphrase if desired.

2. Start the SSH agent:
   ```bash
   eval "$(ssh-agent -s)"
   ```

3. Add your SSH key to the agent:
   ```bash
   ssh-add ~/.ssh/id_ed25519
   ```

> [!NOTE]
> For Windows, use Git Bash or Windows Subsystem for Linux (WSL) to run these commands.

## 📤 Copy Your Public Key to Apollo HPC

4. Display your public key:
   ```bash
   cat ~/.ssh/id_ed25519.pub
   ```

5. Copy the output of the above command.

6. SSH into Apollo HPC:
   ```bash
   ssh your_username@ppxhpcacc01.coh.org
   ```

7. Once logged in, create or edit the `~/.ssh/authorized_keys` file:
   ```bash
   mkdir -p ~/.ssh
   chmod 700 ~/.ssh
   nano ~/.ssh/authorized_keys
   ```

8. Paste your public key into this file, save, and exit (in nano, use Ctrl+X, then Y, then Enter).

9. Set the correct permissions:
   ```bash
   chmod 600 ~/.ssh/authorized_keys
   ```

## 🔒 Test Your Passwordless SSH Connection

10. Exit from Apollo HPC and try to SSH in again:
    ```bash
    ssh your_username@ppxhpcacc01.coh.org
    ```

You should now be able to log in without entering a password.

> [!TIP]
> To make your SSH experience even smoother, you can create an alias in your local `~/.ssh/config` file:
> ```
> Host apollo
>     HostName ppxhpcacc01.coh.org
>     User your_username
>     IdentityFile ~/.ssh/id_ed25519
> ```
> After adding this, you can simply use `ssh apollo` to connect.

> [!IMPORTANT]
> Keep your private key (id_ed25519) secure and never share it with anyone!

Happy computing on the HPC! 🚀
