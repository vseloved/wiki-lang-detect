(in-package #:wiki-lang-detect)
(named-readtables:in-readtable rutilsx-readtable)


(defparameter *internet-lang-bias*
  #h(:en 0.536
     :ru 0.064
     :de 0.056
     :ja 0.051
     :es 0.049
     :fr 0.041
     :zh 0.028
     :pt 0.025
     :it 0.019
     :po 0.017
     :tr 0.015
     :nl 0.013
     :fa 0.009
     :ar 0.008
     :ko 0.007
     :cs 0.007
     :sv 0.005
     :vi 0.004
     :id 0.004
     :el 0.004
     :ro 0.004
     :hu 0.003
     :dk 0.003
     :th 0.003
     :fi 0.002
     :sk 0.002
     :bg 0.002
     :no 0.002)
  "Source: https://en.wikipedia.org/wiki/Languages_used_on_the_Internet")

(defparameter *min-lang-bias* 0.01
  "Minimum expected probability of seeing a text of a certain language.")

(defparameter *iso-639-1*
  #h(:aa "Afar"
     :ab "Abkhazian"
     :af "Afrikaans"
     :ak "Akan"
     :sq "Albanian"
     :am "Amharic"
     :ar "Arabic"
     :an "Aragonese"
     :hy "Armenian"
     :as "Assamese"
     :av "Avaric"
     :ae "Avestan"
     :ay "Aymara"
     :az "Azerbaijani"
     :ba "Bashkir"
     :bm "Bambara"
     :eu "Basque"
     :be "Belarusian"
     :bn "Bengali"
     :bh "Bihari"
     :bi "Bislama"
     :bs "Bosnian"
     :br "Breton"
     :bg "Bulgarian"
     :my "Burmese"
     :ca "Catalan/Valencian"
     :ch "Chamorro"
     :ce "Chechen"
     :zh "Chinese"
     :cu "Church Slavic"
     :cv "Chuvash"
     :kw "Cornish"
     :co "Corsican"
     :cr "Cree"
     :cy "Welsh"
     :cs "Czech"
     :da "Danish"
     :de "German"
     :dv "Maldivian"
     :nl "Dutch/Flemish"
     :dz "Dzongkha"
     :en "English"
     :eo "Esperanto"
     :et "Estonian"
     :eu "Basque"
     :ee "Ewe"
     :fo "Faroese"
     :fa "Persian"
     :fj "Fijian"
     :fi "Finnish"
     :fr "French"
     :fy "Western Frisian"
     :ff "Fulah"
     :ka "Georgian"
     :de "German"
     :gd "Gaelic"
     :ga "Irish"
     :gl "Galician"
     :el "Greek"
     :gn "Guarani"
     :gu "Gujarati"
     :ht "Haitian"
     :ha "Hausa"
     :he "Hebrew"
     :hz "Herero"
     :hi "Hindi"
     :ho "Hiri Motu"
     :hr "Croatian"
     :hu "Hungarian"
     :hy "Armenian"
     :ig "Igbo"
     :is "Icelandic"
     :io "Ido"
     :ii "Sichuan Yi"
     :iu "Inuktitut"
     :ie "Interlingue Occidental"
     :ia "Interlingua"
     :id "Indonesian"
     :ik "Inupiaq"
     :is "Icelandic"
     :it "Italian"
     :jv "Javanese"
     :ja "Japanese"
     :kl "Greenlandic"
     :kn "Kannada"
     :ks "Kashmiri"
     :kr "Kanuri"
     :kk "Kazakh"
     :km "Central Khmer"
     :ki "Kikuyu"
     :rw "Kinyarwanda"
     :ky "Kirghiz"
     :kv "Komi"
     :kg "Kongo"
     :ko "Korean"
     :kj "Kuanyama"
     :ku "Kurdish"
     :lo "Lao"
     :la "Latin"
     :lv "Latvian"
     :li "Limburgan"
     :ln "Lingala"
     :lt "Lithuanian"
     :lb "Luxembourgish"
     :lu "Luba-Katanga"
     :lg "Ganda"
     :mk "Macedonian"
     :mh "Marshallese"
     :ml "Malayalam"
     :mi "Maori"
     :mr "Marathi"
     :ms "Malay"
     :mk "Macedonian"
     :mg "Malagasy"
     :mt "Maltese"
     :mn "Mongolian"
     :mi "Maori"
     :my "Burmese"
     :na "Nauru"
     :nv "Navajo"
     :nr "South Ndebele"
     :nd "North Ndebele"
     :ng "Ndonga"
     :ne "Nepali"
     :nl "Dutch/Flemish"
     :nn "Norwegian Nynorsk"
     :nb "Norwegian Bokmål"
     :no "Norwegian"
     :ny "Chichewa"
     :oc "Occitan"
     :oj "Ojibwa"
     :or "Oriya"
     :om "Oromo"
     :os "Ossetian"
     :pa "Panjabi"
     :fa "Persian"
     :pi "Pali"
     :pl "Polish"
     :pt "Portuguese"
     :ps "Pushto"
     :qu "Quechua"
     :rm "Romansh"
     :ro "Romanian/Moldavian"
     :rn "Rundi"
     :ru "Russian"
     :sg "Sango"
     :sa "Sanskrit"
     :si "Sinhala"
     :sk "Slovak"
     :sl "Slovenian"
     :se "Northern Sami"
     :sm "Samoan"
     :sn "Shona"
     :sd "Sindhi"
     :so "Somali"
     :st "Southern Sotho"
     :es "Spanish"
     :sq "Albanian"
     :sc "Sardinian"
     :sr "Serbian"
     :ss "Swati"
     :su "Sundanese"
     :sw "Swahili"
     :sv "Swedish"
     :ty "Tahitian"
     :ta "Tamil"
     :tt "Tatar"
     :te "Telugu"
     :tg "Tajik"
     :tl "Tagalog"
     :th "Thai"
     :bo "Tibetan"
     :ti "Tigrinya"
     :to "Tonga"
     :tn "Tswana"
     :ts "Tsonga"
     :tk "Turkmen"
     :tr "Turkish"
     :tw "Twi"
     :ug "Uighur"
     :uk "Ukrainian"
     :ur "Urdu"
     :uz "Uzbek"
     :ve "Venda"
     :vi "Vietnamese"
     :vo "Volapük"
     :cy "Welsh"
     :wa "Walloon"
     :wo "Wolof"
     :xh "Xhosa"
     :yi "Yiddish"
     :yo "Yoruba"
     :za "Zhuang"
     :zh "Chinese"
     :zu "Zulu")
  "ISO 639-1 language names.")

(defparameter *1lang-scripts*
  #h(equal
     "Han" :zh
     "Hangul" :ko
     "Hiragana" :ja
     "Katakana" :ja
     "Armenian" :hy
     "Khmer" :km
     "Telugu" :te
     "Tamil" :ta
     "Thai" :th
     "Georgian" :ka
     "Greek" :el
     "Gujarati" :gu
     "Lao" :lo     
     "Malayalam" :ml)
  "Scripts corresponding to a single language.")

(defun iso-lang (iso)
  "Language name for its ISO code."
  (? *iso-639-1* iso))

(defun lang-iso (lang)
  "Iso code of LANG."
  (dotable (iso language *iso-639-1*)
    (when (some ^(string-equal lang %)
                (split #\/ language))
      (return iso))))
