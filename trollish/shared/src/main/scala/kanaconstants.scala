package trollish

object KanaConstants {

  /* utility functions. */
  def bar(s: String) = s + "-"
  def hop(s: String) = "+" + s

  lazy val default = Seq(
    /* redundant tones. */
    "ss$"   -> "ス",

    "aa"    -> bar("a"),
    "ee"    -> bar("e"),
    "ii"    -> bar("i"),
    "oo"    -> bar("o"),
    "uu"    -> bar("u"),

    "bb"    -> hop("b"),
    "cc"    -> hop("c"),
    "dd"    -> hop("d"),
    "ff"    -> hop("f"),
    "gg"    -> hop("g"),
    "kk"    -> hop("k"),
    "pp"    -> hop("p"),
    "ss"    -> hop("s"),
    "tt"    -> hop("t"),
    "vv"    -> "v",
    "ww"    -> "w",
    "yy"    -> "y",
    "zz"    -> "z",

    "mm"    -> "nm",
    "rr"    -> "ar",
    "ll"    -> "ル",
    "nn"    -> "ン",

    "x"     -> "ks",

    /* closings. */
    "cch$"  -> "ch",
    "ch$"   -> "フ",
    "gh$"   -> "フ",

    /* normalizes */
    "gha"   -> "ガ",
    "ghi"   -> "ギ",
    "ghu"   -> "グ",
    "ghe"   -> "ゲ",
    "gho"   -> "ゴ",

    "rha"   -> "ra",
    "rhi"   -> "ri",
    "rhu"   -> "ru",
    "rhe"   -> "re",
    "rho"   -> "ro",
    "rh"    -> "r",
    "ra"    -> "ラ",
    "ri"    -> "リ",
    "ru"    -> "ル",
    "re"    -> "レ",
    "ro"    -> "ロ",
    "ir"    -> "ia",
    "ur"    -> "ua",
    "er"    -> "ea",
    "ar"    -> "aル",
    "qu"    -> "kw",
    "tya"   -> "チャ",
    "tyi"   -> "チィ",
    "tyu"   -> "チュ",
    "tye"   -> "チェ",
    "tyo"   -> "チョ",
    "sha"   -> "シャ",
    "shi"   -> "シ",
    "shu"   -> "シュ",
    "she"   -> "シェ",
    "sho"   -> "ショ",
    "tha"   -> "サ",
    "thi"   -> "シ",
    "thu"   -> "ス",
    "the"   -> "セ",
    "tho"   -> "ソ",
    "th"    -> "ス",
    "cha"   -> "ハ",
    "chi"   -> "ヒ",
    "chu"   -> "フ",
    "che"   -> "ヘ",
    "cho"   -> "ホ",
    "ch"    -> "ヒ",
    "dha"   -> "ザ",
    "dhi"   -> "ジ",
    "dhu"   -> "ズ",
    "dhe"   -> "ゼ",
    "dho"   -> "ゾ",
    "dh"    -> "ズ",
    "lha"   -> "ラ",
    "lhi"   -> "リ",
    "lhu"   -> "ル",
    "lhe"   -> "レ",
    "lho"   -> "ロ",
    "hwa"   -> "ホワ",
    "hwi"   -> "フイ",
    "hwu"   -> "フウ",
    "hwe"   -> "フエ",
    "hwo"   -> "フオ",
    "hya"   -> "ヒャ",
    "hyi"   -> "ヒィ",
    "hyu"   -> "ヒュ",
    "hye"   -> "ヒェ",
    "hyo"   -> "ヒョ",
    "ba"    -> "バ",
    "bi"    -> "ビ",
    "bu"    -> "ブ",
    "be"    -> "ベ",
    "bo"    -> "ボ",
    "ca"    -> "カ",
    "ci"    -> "キ",
    "cu"    -> "ク",
    "ce"    -> "ケ",
    "co"    -> "コ",
    "da"    -> "ダ",
    "di"    -> "ディ",
    "du"    -> "ドゥ",
    "de"    -> "デ",
    "do"    -> "ド",
    "fa"    -> "ファ",
    "fi"    -> "フィ",
    "fu"    -> "ファ",
    "fe"    -> "フェ",
    "fo"    -> "フォ",
    "pha"   -> "ファ",
    "phi"   -> "フィ",
    "phe"   -> "フェ",
    "pho"   -> "フォ",
    "phu"   -> "ファ",
    "ph"    -> "フ",
    "ga"    -> "ガ",
    "gi"    -> "ギ",
    "gu"    -> "グ",
    "ge"    -> "ゲ",
    "go"    -> "ゴ",
    "ha"    -> "ハ",
    "hi"    -> "ヒ",
    "hu"    -> "フ",
    "he"    -> "ヘ",
    "ho"    -> "ホ",
    "ja"    -> "ジャ",
    "ji"    -> "ジ",
    "ju"    -> "ジュ",
    "je"    -> "ジェ",
    "jo"    -> "ジョ",
    "ka"    -> "カ",
    "ki"    -> "キ",
    "ku"    -> "ク",
    "ke"    -> "ケ",
    "ko"    -> "コ",
    "la"    -> "ラ",
    "li"    -> "リ",
    "lu"    -> "ル",
    "le"    -> "レ",
    "lo"    -> "ロ",
    "ma"    -> "マ",
    "mi"    -> "ミ",
    "mu"    -> "ム",
    "me"    -> "メ",
    "mo"    -> "モ",
    "na"    -> "ナ",
    "ni"    -> "ニ",
    "nu"    -> "ヌ",
    "ne"    -> "ネ",
    "no"    -> "ノ",
    "pa"    -> "パ",
    "pi"    -> "ピ",
    "pu"    -> "プ",
    "pe"    -> "ペ",
    "po"    -> "ポ",
    "qu"    -> "ク",
    "sa"    -> "サ",
    "si"    -> "シ",
    "su"    -> "ス",
    "se"    -> "セ",
    "so"    -> "ソ",
    "ta"    -> "タ",
    "ti"    -> "ティ",
    "tu"    -> "トゥ",
    "te"    -> "テ",
    "to"    -> "ト",
    "va"    -> "ヴァ",
    "vi"    -> "ヴィ",
    "vu"    -> "ヴ",
    "ve"    -> "ヴェ",
    "vo"    -> "ヴォ",
    "wa"    -> "ワ",
    "wi"    -> "ウィ",
    "wu"    -> "ウ",
    "we"    -> "ウェ",
    "wo"    -> "ウォ",
    "ya"    -> "ヤ",
    "yu"    -> "ユ",
    "yo"    -> "ヨ",
    "za"    -> "ザ",
    "zi"    -> "ジ",
    "zu"    -> "ズ",
    "ze"    -> "ゼ",
    "zo"    -> "ゾ",
    "b"     -> "ブ",
    "c"     -> "ク",
    "d"     -> "ド",
    "f"     -> "フ",
    "g"     -> "グ",
    "h"     -> "フ",
    "j"     -> "ジュ",
    "k"     -> "ク",
    "l"     -> "ル",
    "m"     -> "ム",
    "n"     -> "ン",
    "p"     -> "プ",
    "q"     -> "ク",
    "r"     -> "ル",
    "s"     -> "ス",
    "t"     -> "ト",
    "v"     -> "ヴ",
    "w"     -> "ウ",
    "y"     -> "イ",
    "z"     -> "ズ",
    "a"     -> "ア",
    "i"     -> "イ",
    "u"     -> "ウ",
    "e"     -> "エ",
    "o"     -> "オ",
    "-"     -> "ー"
  )
}
