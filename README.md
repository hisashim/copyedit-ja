Copyedit-ja - Copyediting tool for Japanese text
========

Copyedit-ja is a copyediting tool for Japanese text on Emacs.

  * Copyright (c) 2008-2021 Hisashi Morita
  * License: Public Domain

## Synopsis

  1. Check possible typos.
     ```
     M-x copyedit-ja-check-typo-particles
     ```
  2. Edit them.
     ```
     M-x copyedit-ja-normalize-typo-particles
     ```

(See color-moccur for detail, since Copyedit-ja is basically just a
wrapper around color-moccur with preset regexp patterns.)

## Features

Copyedit-ja helps checking and resolving issues with:

  * Possible typos (successive ga, no, ni, wo, etc.)
  * Character widths (numerals, parentheses, etc.)
  * Matching of opening and closing parentheses
  * Style (direct style (da-dearu) and distal style (desu-masu))
  * Commas and periods (ten-maru and kan-piri)
  * Uses of prolonged sound marks (onbiki)
  * Space characters between latin text and Japanese text,

etc.

Try `M-x copyedit-ja- TAB` to list available commands.

## Requirements

  * color-moccur: multi-buffer occur (grep) mode
    - v2.71 (MELPA):
      [https://melpa.org/#/color-moccur](https://melpa.org/#/color-moccur)
      (source: [https://github.com/myuhe/color-moccur.el](https://github.com/myuhe/color-moccur.el))
    - v2.73 from the original author:
      [https://web.archive.org/web/20180527232131/www.bookshelf.jp/elc/color-moccur.el](https://web.archive.org/web/20180527232131/www.bookshelf.jp/elc/color-moccur.el)
  * MeCab: Yet another Japanese morphological analyzer (optional)
    - [https://github.com/taku910/mecab](https://github.com/taku910/mecab)

## Installation

  1. Install requirements.
     - Install color-moccur using Emacs package manager.
       (See [https://melpa.org](https://melpa.org) for instructions.)
     - Install MeCab and its UTF-8 dictionary, e.g.
       ```
       $ sudo apt install mecab mecab-ipadic-utf8
       ```

  2. Install copyedit-ja.
     - Put copyedit-ja.el somewhere in your load-path, e.g.
       ```
       $ cp copyedit-ja.el ~/.emacs.d/lisp/
       ```
     - If you prefer, load copyedit-ja on startup, e.g.
       ```
       $ echo "(require 'copyedit-ja)" >> ~/emacs.d/init.el
       ```

  3. Configure copyedit-ja (optional).
     - Tweak dictionaries to fit your needs in your init file, e.g.
       ```
       (setq copyedit-ja--dict-user-specified-keywords
             (append copyedit-ja--dict-user-specified-keywords
                     '(("TCP-IP". "TCP/IP"))))
       ```

## References

Copyedit-ja is inspired and heavily influenced by respectable forerunners' works.

  * [KSU1989]
    Masanori Kurata, Akira Suganuma, Kazuo Ushijima,
    "Development of a System of Writing Tools for Japanese Documents on
    a Personal Computer.",
    Computer Software, Vol. 6, No. 4 (1989), pp. 55-67.
      - [https://ci.nii.ac.jp/naid/110003743558](https://ci.nii.ac.jp/naid/110003743558)
      - [https://www.jstage.jst.go.jp/article/jssst/6/4/6_4_373/_article/-char/ja](https://www.jstage.jst.go.jp/article/jssst/6/4/6_4_373/_article/-char/ja)
  * [Urayama1998]
    Takeshi Urayama, Denshi Henshu no Susume: sed no Katsuyou
    (E-editing using Sed), 1998, Doseisha.
      - [https://ci.nii.ac.jp/ncid/BA38403160](https://ci.nii.ac.jp/ncid/BA38403160)
