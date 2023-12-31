use crate::{detect, FileType, FileTypeResolver};

#[rustfmt::skip]
pub(crate) const PATH_SUFFIX: &[(&str, FileTypeResolver)] = &[
    ("etc/a2ps.cfg", FileTypeResolver::Static(FileType::A2ps)),
    ("usr/share/alsa/alsa.conf", FileTypeResolver::Static(FileType::AlsaConf)),
    ("etc/asound.conf", FileTypeResolver::Static(FileType::AlsaConf)),
    (".aptitude/config", FileTypeResolver::Static(FileType::AptConf)),
    ("etc/cdrdao.conf", FileTypeResolver::Static(FileType::CdrdaoConf)),
    ("etc/default/cdrdao", FileTypeResolver::Static(FileType::CdrdaoConf)),
    ("etc/defaults/cdrdao", FileTypeResolver::Static(FileType::CdrdaoConf)),
    ("debian/changelog", FileTypeResolver::Static(FileType::DebChangelog)),
    ("debian/control", FileTypeResolver::Static(FileType::DebControl)),
    ("debian/copyright", FileTypeResolver::Static(FileType::DebCopyright)),
    ("etc/apt/sources.list", FileTypeResolver::Static(FileType::DebSources)),
    ("etc/DIR_COLORS", FileTypeResolver::Static(FileType::DirColors)),
    ("etc/dnsmasq.conf", FileTypeResolver::Static(FileType::DnsMasq)),
    ("etc/yum.conf", FileTypeResolver::Static(FileType::DosIni)),
    ("etc/pacman.conf", FileTypeResolver::Static(FileType::ConfIni)),
    (".gnupg/gpg.conf", FileTypeResolver::Static(FileType::Gpg)),
    (".gnupg/options", FileTypeResolver::Static(FileType::Gpg)),
    ("var/backups/gshadow.bak", FileTypeResolver::Static(FileType::Group)),
    ("etc/gshadow", FileTypeResolver::Static(FileType::Group)),
    ("etc/group-", FileTypeResolver::Static(FileType::Group)),
    ("etc/gshadow.edit", FileTypeResolver::Static(FileType::Group)),
    ("etc/gshadow-", FileTypeResolver::Static(FileType::Group)),
    ("etc/group", FileTypeResolver::Static(FileType::Group)),
    ("var/backups/group.bak", FileTypeResolver::Static(FileType::Group)),
    ("etc/group.edit", FileTypeResolver::Static(FileType::Group)),
    ("boot/grub/menu.lst", FileTypeResolver::Static(FileType::Grub)),
    ("etc/grub.conf", FileTypeResolver::Static(FileType::Grub)),
    ("boot/grub/grub.conf", FileTypeResolver::Static(FileType::Grub)),
    ("etc/host.conf", FileTypeResolver::Static(FileType::HostConf)),
    ("etc/hosts.allow", FileTypeResolver::Static(FileType::HostsAccess)),
    ("etc/hosts.deny", FileTypeResolver::Static(FileType::HostsAccess)),
    (".icewm/menu", FileTypeResolver::Static(FileType::IceMenu)),
    ("etc/libao.conf", FileTypeResolver::Static(FileType::Libao)),
    ("etc/limits", FileTypeResolver::Static(FileType::Limits)),
    ("etc/login.access", FileTypeResolver::Static(FileType::LoginAccess)),
    ("etc/login.defs", FileTypeResolver::Static(FileType::LoginDefs)),
    ("etc/aliases", FileTypeResolver::Static(FileType::MailAliases)),
    ("etc/mail/aliases", FileTypeResolver::Static(FileType::MailAliases)),
    ("etc/man.conf", FileTypeResolver::Static(FileType::ManConf)),
    ("etc/conf.modules", FileTypeResolver::Static(FileType::ModConf)),
    ("etc/modules", FileTypeResolver::Static(FileType::ModConf)),
    ("etc/modules.conf", FileTypeResolver::Static(FileType::ModConf)),
    (".mplayer/config", FileTypeResolver::Static(FileType::MPlayerConf)),
    ("etc/nanorc", FileTypeResolver::Static(FileType::Nanorc)),
    ("etc/pam.conf", FileTypeResolver::Static(FileType::PamConf)),
    ("var/backups/passwd.bak", FileTypeResolver::Static(FileType::Passwd)),
    ("var/backups/shadow.bak", FileTypeResolver::Static(FileType::Passwd)),
    ("etc/passwd", FileTypeResolver::Static(FileType::Passwd)),
    ("etc/passwd-", FileTypeResolver::Static(FileType::Passwd)),
    ("etc/shadow.edit", FileTypeResolver::Static(FileType::Passwd)),
    ("etc/shadow-", FileTypeResolver::Static(FileType::Passwd)),
    ("etc/shadow", FileTypeResolver::Static(FileType::Passwd)),
    ("etc/passwd.edit", FileTypeResolver::Static(FileType::Passwd)),
    ("etc/pinforc", FileTypeResolver::Static(FileType::PInfo)),
    ("etc/protocols", FileTypeResolver::Static(FileType::Protocols)),
    ("etc/sensors3.conf", FileTypeResolver::Static(FileType::Sensors)),
    ("etc/sensors.conf", FileTypeResolver::Static(FileType::Sensors)),
    ("etc/services", FileTypeResolver::Static(FileType::Services)),
    ("etc/serial.conf", FileTypeResolver::Static(FileType::SetSerial)),
    ("etc/udev/cdsymlinks.conf", FileTypeResolver::Static(FileType::Sh)),
    ("etc/profile", FileTypeResolver::Dynamic(|_, content| detect::sh(content, None))),
    ("etc/slp.conf", FileTypeResolver::Static(FileType::SlpConf)),
    ("etc/slp.reg", FileTypeResolver::Static(FileType::SlpReg)),
    ("etc/slp.spi", FileTypeResolver::Static(FileType::SlpSpi)),
    ("etc/sudoers", FileTypeResolver::Static(FileType::Sudoers)),
    ("etc/sysctl.conf", FileTypeResolver::Static(FileType::Sysctl)),
    (".cargo/config", FileTypeResolver::Static(FileType::Toml)),
    (".cargo/credentials", FileTypeResolver::Static(FileType::Toml)),
    ("etc/udev/udev.conf", FileTypeResolver::Static(FileType::UdevConf)),
    ("etc/updatedb.conf", FileTypeResolver::Static(FileType::UpdateDb)),
    ("etc/xinetd.conf", FileTypeResolver::Static(FileType::Xinetd)),
    ("etc/blkid.tab", FileTypeResolver::Static(FileType::Xml)),
    ("etc/blkid.tab.old", FileTypeResolver::Static(FileType::Xml)),
    ("etc/zprofile", FileTypeResolver::Static(FileType::Zsh)),
    ("etc/zsh/zprofile", FileTypeResolver::Static(FileType::Zsh)),
];
