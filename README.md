Copyedit-ja: Copyediting tool for Japanese text
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

  * Emacs

  * color-moccur: multi-buffer occur (grep) mode [1]
    - [https://melpa.org/#/color-moccur](https://melpa.org/#/color-moccur)
      (source: [https://github.com/myuhe/color-moccur.el](https://github.com/myuhe/color-moccur.el)
      (forked from v2.71 of the original and incorporated fixes and improvements))

  * MeCab: Yet another Japanese morphological analyzer. (optional)
    - [https://github.com/taku910/mecab](https://github.com/taku910/mecab)
    - Dictionary for MeCab.
      - [https://osdn.net/projects/naist-jdic/](https://osdn.net/projects/naist-jdic/)
      - [https://sourceforge.net/projects/mecab/](https://sourceforge.net/projects/mecab/)

[1]: color-moccur [v2.73](https://web.archive.org/web/20110224015820/http://www.bookshelf.jp:80/elc/color-moccur.el)
from the original author can be found in the Internet Archive, although the
code seems the same as
[v2.71](https://web.archive.org/web/20100719125327/http://www.bookshelf.jp:80/elc/color-moccur.el).

## Installation

  1. Install requirements.
     - Install Emacs.
       ```
       $ sudo apt install emacs
       ```
     - Install color-moccur using Emacs package manager.
       (See [MELPA](https://melpa.org) for detail.)
       ```
       $ emacs ~/emacs.d/init.el
       ...(add package configurations and reload)...
       M-x package-list-packages
       M-x package install
       ...(choose color-moccur)...
       ```
     - Install MeCab and UTF-8 dictionary for it.
       ```
       $ sudo apt install mecab mecab-naist-jdic
       ```

  2. Install copyedit-ja.
     - Put `copyedit-ja.el` somewhere in your load-path.
       ```
       $ cp copyedit-ja.el ~/.emacs.d/lisp/
       ```
     - If you prefer, load `copyedit-ja` on startup.
       ```
       $ echo "(require 'copyedit-ja)" >> ~/emacs.d/init.el
       ```

  3. Configure copyedit-ja (optional).
     - Tweak dictionaries to fit your needs in your Emacs init file.
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
