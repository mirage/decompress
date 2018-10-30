  type t = FAT | AMIGA | VMS | UNIX | VM_CMS | ATARI | HPFS | MAC | Z_SYS |
  CP_M | TOPS | NTFS | QDOS | ACORN | UNKNOWN

  let default = UNKNOWN

  let of_int = function
  | 0 -> Some FAT
  | 1 -> Some AMIGA
  | 2 -> Some VMS
  | 3 -> Some UNIX
  | 4 -> Some VM_CMS
  | 5 -> Some ATARI
  | 6 -> Some HPFS
  | 7 -> Some MAC
  | 8 -> Some Z_SYS
  | 9 -> Some CP_M
  | 10 -> Some TOPS
  | 11 -> Some NTFS
  | 12 -> Some QDOS
  | 13 -> Some ACORN
  | 255 -> Some UNKNOWN
  | _ -> None

  let to_int = function
  | FAT -> 0
  | AMIGA -> 1
  | VMS -> 2
  | UNIX -> 3
  | VM_CMS -> 4
  | ATARI -> 5
  | HPFS -> 6
  | MAC -> 7
  | Z_SYS -> 8
  | CP_M -> 9
  | TOPS -> 10
  | NTFS -> 11
  | QDOS -> 12
  | ACORN -> 13
  | UNKNOWN -> 255

  let to_string = function
  | FAT -> "FAT filesystem (MS-DOS, OS/2, NT/Win32)"
  | AMIGA -> "Amiga"
  | VMS -> "VMS (or OpenVMS)"
  | UNIX -> "Unix"
  | VM_CMS -> "VM/CMS"
  | ATARI -> "Atari TOS"
  | HPFS -> "HPFS filesystem (OS/2, NT)"
  | MAC -> "Macintosh"
  | Z_SYS -> "Z-System"
  | CP_M -> "CP/M"
  | TOPS -> "TOPS-20"
  | NTFS -> "NTFS filesystem (NT)"
  | QDOS -> "QDOS"
  | ACORN -> "Acorn RISCOS"
  | UNKNOWN -> "unknown"
