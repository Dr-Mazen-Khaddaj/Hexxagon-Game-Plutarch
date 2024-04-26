# Installing Nix and Configuring `nix.conf`

This guide provides instructions for installing Nix and setting up `nix.conf` to support Nix Flakes and other advanced features necessary for the Hexxagon Game on Cardano project.

## Installing Nix

### Official Option

To install Nix using the official installer, follow the steps below or visit [Nix Official Download Page](https://nixos.org/download.html):

```bash
sh <(curl -L https://nixos.org/nix/install) --daemon
```
### Preferred option
- If you are setting up Nix on your system for the first time, try Determinate Systems' **[Zero-to-Nix](https://zero-to-nix.com)** instead of the official installer, as it provides an easier tool for **[installing](https://zero-to-nix.com/start/install)** and **[uninstalling](https://zero-to-nix.com/start/uninstall)** Nix.
- Learn more about this method at [Determinate Systems](https://zero-to-nix.com/concepts/nix-installer)

```
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```

## Configuring `nix.conf`

- After installing Nix, you need to configure `nix.conf`.
- This configuration tells Nix to use the Nix binary cache at https://cache.nixos.org/ and enables experimental features such as Nix flakes and ca derivations.
- Configuration steps vary slightly depending on your operating system.

### On non-NixOS systems

Edit file `/etc/nix/nix.conf` with the following content:

```
build-users-group = nixbld
experimental-features = nix-command flakes ca-derivations
substituters = https://cache.nixos.org/
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
trusted-users = root <your-username>
```
Replace `<your-username>` with your actual username. After editing, restart the Nix daemon:
```
sudo systemctl restart nix-daemon.service
```

### MacOS

On MacOS, edit the `nix.conf` with additional settings to adapt to the MacOS environment:

```
build-users-group = nixbld
experimental-features = nix-command flakes ca-derivations
substituters = https://cache.nixos.org/
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
trusted-users = <your-username>

system = x86_64-darwin
extra-platforms = x86_64-darwin aarch64-darwin
sandbox = false
extra-sandbox-paths = /System/Library/Frameworks /System/Library/PrivateFrameworks /usr/lib /private/tmp /private/var/tmp /usr/bin/env
```
Replace `<your-username>` with your actual username. After editing, save the changes and restart your Mac to apply these settings.

### On NixOS system

If you are using NixOS, edit your system configuration file `/etc/nixos/configuration.nix`:

```
 nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes ca-derivations
    '';
  };
  nix.settings.trusted-users = [ "root" "<your-username>"];
```

Replace `<your-username>` with your actual username and apply the changes:

```
sudo nixos-rebuild switch
```

## Done
**Finally, When finished installing Nix, close the terminal session and open a fresh one.**