# hnotif

hnotif is a minimal notification manager for Linux, written in Haskell. It's very much in its early stages of development with only basic functionality for displaying notifications. The goal is to provide a customizable notification manager, configurable in Haskell. Right now, only basic configuration is available.

I'm writing this project mainly for fun but it would be awesome if this project becomes useful to someone in the future (even if that someone might just be me).

# usage

If you want to use this project, invoke `hnotif defHNotifConfig` from a Haskell file.

For an example, see `app/Main.hs`. You can modify the configuration by providing record updates to the `defHNotifConfig`.

For a list of currently supported options, see `src/System/HNotif/Configuration.hs` where the data type `HNotifConfig` is defined with some additional info.

# features

- notification service compatible with v1.3 of the org.freedesktop.Notification service
- written and configurable in Haskell (basic as of now)

# plans

- the look of the notification must be fully configurable in Haskell
- ability to close a notification by pressing a button
- more features provided by the dbus protocol (hyperlinks, markup, ...)
- ...

# contributing

Contributions are always welcome! I'm especially inexperienced in GTK, so feel free to open issues, discuss the code or even submit a pull request.