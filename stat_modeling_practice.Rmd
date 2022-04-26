---
title: "統計学という道具を手に入れよう"
author: "Shinichiro Tanimae"
date: '2022-04-25'
output:
  html_document:
    df_print: paged
  pdf_document:
    highlight: haddock
    latex_engine: xelatex
    dev: cairo_pdf
    keep_tex: yes
documentclass: bxjsarticle
header-includes:
- \renewcommand{\eqref}{\ref}
- \usepackage{zxjatype}
- \usepackage[ipa]{zxjafont}
side: oneside
# fontsize: 40pt
# papersize: a4paper
linestretch: 1.25
widowpenalty: 10000
clubpenalty: 10000
editor_options:
  chunk_output_type: console
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  # dev = "cairo_pdf",
  # dev.args = list(family = "ipaexg"),
  echo = FALSE,
  eval = TRUE,
  fig.width = 2*80/25.4,
  fig.height = 2*80/25.4*3/4,
  # fig.asp = 0.8,
  out.width = "80%",
  fig.align = "center", 
  out.extra = "",
  warning = FALSE, 
  message=FALSE,
  error = FALSE, 
  autodep = TRUE,
  cache = TRUE)

options(tinytex.latexmk.emulation = FALSE, width = 80)
options(knitr.kable.NA = '', texi2dvi = "xetex")
options(kableExtra.latex.load_packages = FALSE, tidyverse.quiet = TRUE)
Sys.setlocale("LC_TIME", "en_US.UTF-8") # アメリカ英語に設定
```

```{r read-packages, include = FALSE}
# 必要なパッケージ
library(magrittr)
library(tidyverse)
library(lubridate)
library(knitr)
library(kableExtra)
library(broom)
library(lemon)
library(magick)
library(gnnlab)

dir("~/Lab_Data/tanimaes/share_files/GEP_study_photo/")

```

```{r, set, include=FALSE}
DIR = dir("~/Lab_Data/tanimaes/share_files/modeling_manual/set_images/",
          pattern = "[JjPp][PpNn][Gg]", full = T)
```

```{r stat00}
# knitr::include_graphics("~/Lab_Data/tanimaes/share_files/modeling_manual/set_images/top01.png")
```

<br><br>

<link href="tidy_style.css" rel="stylesheet"></link>

<style type="text/css">body{font-size: 12pt;}</style>

<br>

統計モデリングは身の回りの様々な場面で活躍しています.
特に,現状の把握と過去・未来の予想が得意です.

**・天気予報**

**・電力需要予想**

**・店頭,自販機でのラインナップ**

**・医薬品,肥料などの適正使用量**

**・郵便物の到着予想日時**

**・大地震の発生確率**

<br>

このように,すでに多くの場面でデータから合理的な判断が提供されています.
さらなる技術発展が進めば,以下のような内容も可能になるかもしれません.

**・アルバイト従業員数の最適化**

**・タクシー配車数,配車位置の最適化**

**・ワクチン接種の必要性を個別に判定**

**・ムラサキダコの来遊時期予想**

<br>

重要なポイントは, 統計学自体が目的として存在する場面は少なく, あくまで別の目的を達成するための道具として存在する点です.
時には人の直感なども考慮しながら, 合理的な解釈を提供できるのが統計モデリングの魅力です.

しかし, 以下のようなケースはどうでしょうか.

<br>

```{r stat01, fig.cap = CAP01}
CAP01 = "面白いデータを持っているからこそ, 表現方法が疎かだと勿体ない."
knitr::include_graphics(DIR |> str_subset("modeling_002"))
```

<br>

私の友人である A 君は, 回帰直線の上に 2 個の半透明な長方形の図形をくっつけて,
時間経過とともに代謝速度の減少が緩やかになることを表現しました.
この解釈は正しいかもしれませんが, 誰も使っていない手法なので,
説明を求められたときに回答することが難しくなります.

また, 仮に解析手法が正しい場合でも, 統計解析の**ブラックボックス化**はよく見られます.
説明を求められたときに説明できることが大切です.
ここで少し, 上のデータについて簡単に解析しなおしてみます.

<div class="ac-box">
<input id="ac-1" name="accordion-1" type="checkbox" />
<label for="ac-1"> Rのコードを確認する </label>
<div class="ac-small"><p>

```{r, stat02, echo=T, eval=F}
# パッケージの読み込み.
library(tidyverse)
library(lubridate)

# データの整形.
df = readxl::read_xlsx("~/Lab_Data/tanimaes/share_files/example_data/Bathynomus_oxygen_consumption_rate.xlsx",
                       skip = 0, col_names = paste0("col", 1:34)) |> 
  mutate(across(everything(), ~ as.character(.x)))

df = df |>
  mutate(cname = c("date", "expA_id", "expB_id", "expA_weight", "expB_weight",
                   "expA_t1", "expB_t1", "expA_t2", "expB_t2", "expA_t3", "expB_t3")) |> 
  select(-c(col1, col2)) |> 
  pivot_longer(col3:col34, names_to = "name",values_to = "val") |>
  pivot_wider(names_from = cname, values_from = val)

df1 = df |>
  mutate(date = as_date(as.numeric(date), origin = "1900-01-01")) |> 
  mutate(across(matches("_[w, t]"), ~ as.double(.x))) |> 
  pivot_longer(cols = matches("exp[A, B]"),
               names_to = c("Exp", ".value"),
               names_sep = "_") |> 
  filter(t1>0 & t2>0 & t3>0) |> 
  mutate(elapsed_day = as.double(date - min(date)),
         mean_val = (t1+t2+t3)/3)

# 作図.
df1 |>
  ggplot(aes(elapsed_day, mean_val)) +
  geom_point(size = 3) +
  geom_smooth(formula = y ~ x, method = "glm", 
              method.args = list(family = "Gamma"),
              size = 2, color = "turquoise4", fill = "turquoise4") +
  ggtitle("オオグソクムシの絶食に対する代謝速度変化") +
  scale_x_continuous(name = "絶食日数",
                     breaks = seq(0, 100, by = 20), limits = c(0, 100)) +
  scale_y_continuous(name = expression("MO"[2]~"(mg"~"H"^{-1}~"Kg"^{-1}~")"),
                     breaks = seq(0, 70, by = 10), limits = c(0, 70)) +
  ggpubr::theme_pubr()

```

</p></div></div>

<br>

```{r, stat03, echo=F, eval=T}
# パッケージの読み込み.
library(tidyverse)
library(lubridate)

# データの整形.
df = readxl::read_xlsx("~/Lab_Data/tanimaes/share_files/example_data/Bathynomus_oxygen_consumption_rate.xlsx",
                       skip = 0, col_names = paste0("col", 1:34)) |> 
  mutate(across(everything(), ~ as.character(.x)))

df = df |>
  mutate(cname = c("date", "expA_id", "expB_id", "expA_weight", "expB_weight",
                   "expA_t1", "expB_t1", "expA_t2", "expB_t2", "expA_t3", "expB_t3")) |> 
  select(-c(col1, col2)) |> 
  pivot_longer(col3:col34, names_to = "name",values_to = "val") |>
  pivot_wider(names_from = cname, values_from = val)

df1 = df |>
  mutate(date = as_date(as.numeric(date), origin = "1900-01-01")) |> 
  mutate(across(matches("_[w, t]"), ~ as.double(.x))) |> 
  pivot_longer(cols = matches("exp[A, B]"),
               names_to = c("Exp", ".value"),
               names_sep = "_") |> 
  filter(t1>0 & t2>0 & t3>0) |> 
  mutate(elapsed_day = as.double(date - min(date)),
         mean_val = (t1+t2+t3)/3)

# 作図.
df1 |>
  ggplot(aes(elapsed_day, mean_val)) +
  geom_point(size = 3) +
  geom_smooth(formula = y ~ x, method = "glm", 
              method.args = list(family = "Gamma"),
              size = 2, color = "turquoise4", fill = "turquoise4") +
  ggtitle("オオグソクムシの絶食に対する代謝速度変化") +
  scale_x_continuous(name = "絶食日数",
                     breaks = seq(0, 100, by = 20), limits = c(0, 100)) +
  scale_y_continuous(name = expression("MO"[2]~"(mg"~"H"^{-1}~"Kg"^{-1}~")"),
                     breaks = seq(0, 70, by = 10), limits = c(0, 70)) +
  ggpubr::theme_pubr()

```

<br>

プロットは実測値, ラインはモデルの期待値, 網掛けは期待値の 95% 信頼区間です.
時間経過に対するオオグソクムシの代謝速度の減少は, 限りなく緩やかであることが分かります.
このように, 統計モデリングによって現象を説明することが出来しました.

<br><br>

## <span>01</span>**世間話**

<br>

データ解析というと, 
[パラメトリック](https://data-science.gr.jp/theory/tbs_parametric.html)
な統計モデリングのことだけを指すわけではなく, 
ノンパラメトリックな手法もたくさんあります.
ここでは基本的にパラメトリックなものしか出てきませんが, 
最後の方にノンパラメトリックな手法についても少し記述しようと思います.

以下には, よく見かける統計モデルを紹介します.

<ul class="msr_flow02">
<li>線形モデル(LM, 単回帰)</li>
<li class="active1">線形モデル(LM, 重回帰)</li>
<li class="active2">一般化線形モデル(GLM)</li>
<li class="active3">一般化線形混合モデル(GLMM)</li>
<li class="active4">階層ベイズモデル(Bayes-HM)</li>
</ul>

<br>

右側に行けば行くほど, 実装や解釈が複雑になっていきます.
卒論・修論を傍聴すると, **LM** や **GLM** がよく登場しますが, まれに **GLMM** を目にする機会もあります.
**GLMM** と **階層ベイズモデル** は
[ほぼ同じ](https://kuboweb.github.io/-kubo/stat/2015/jssp/kubo2015jssp.pdf)
です.

<br>

<h4 class="heading01">**線形モデル(LM)**</h4>

<br>

応答変数が正規分布に従う場合に使用します. 実用的な場面は少ない印象です.

<br>

<h4 class="heading01">**一般化線形混合モデル(GLM)**</h4>

<br>

正規分布以外の分布にも使えるように拡張したモデル.
**分布**って分かりにくいですが, 厳密な定義などは一旦置いといて,
応答変数がどのような値を取りうるのかをよく考えてみましょう.

<br>

<div class="box22"><p>

確率分布いろいろ

<br>

・個体数や回数などの度数 ... 正の整数(離散型の変数) → **ポアソン分布**？

・重量や時間 ... 正の実数(連続型の変数, つまり小数点以下の値もある) → **ガンマ分布**？

・抱卵や発芽の有無 ... 0か1か(離散型の変数) → **ベルヌーイ分布**？

・50 匹の魚について抱卵の有無を 1 匹づつ確認... ベルヌーイ試行の繰り返し(離散型の変数) → **二項分布**？

・その日釣れた魚の総重量 ... 正の実数 or 0 (連続型の変数だけど, 釣れなかった日は 0) → **ハードルガンマ分布**？

・標識再捕法で初めの標識個体が確認されたときの全捕獲個体数 ... 正の整数(離散型の変数) → **幾何分布**？

</p></div>

<br>

<h4 class="heading01">**一般化線形混合モデル(GLMM)**</h4>

<br>

<!-- <span style="color: Navy">**一般化線形混合モデル**</span>  -->

複数のまとまり（地域・種類・季節など）から情報が得られたとき,
各まとまりごとに直線を推定するのが妥当だと判断できる場面があります.
例えば, x軸に年齢, y軸に身長をとった散布図は, 
日本人とロシア人と中国人ではそれぞれ僅かに異なる形をしているんじゃないか, というようなことです.
このような場合は, 共通の事前分布を持つように階層的なモデル構造を生じさせることで,
似たような傾向を取り入れながらも, 個別の特徴 (変量効果) を反映することができます.
より現実的なモデルです.

<br>

そして, モデル式の記述の仕方によって, いろいろなことを考慮していくことが出来ます.

<br>

<h4 class="heading02">**単回帰**</h4>

<br>

1 つの説明変数によって説明される ... 

$$樹木の幹の太さ = 年齢 + (切片)$$

<br>

<h4 class="heading02">**重回帰**</h4>

<br>

複数の説明変数によって説明される ...

$$テストの点数 = 勉強時間 + 体の調子 + (切片)$$

<br>

<h4 class="heading02">**交互作用**</h4>

<br>

複数の説明変数同士の交互作用項 (相乗効果みたいなもの) によって説明される ...

$$ビールの売り上げ = 気温 * 曜日 + (切片)$$

<br>

<h4 class="heading02">**変量効果**</h4>

<br>

条件による切片の変量効果を考慮する ...

$$体重 = 食事の量 + (切片 | 出身国)$$

条件による切片と説明変数の変量効果を考慮する ...

$$酸素フラックス = 水温 + (切片 + 風速 | 季節) + (切片 | 季節)$$

<br>

このように, 様々なことを考慮しながら複雑な統計モデルを組み立てることは可能です.
しかしながら, 予測精度と説明力はトレードオフの関係にあります.
つまり, 説明変数を増やせば説明力 (モデルの当てはまり) は良くなりますが, 
予測精度は下がっていってしまいます.
したがって, なるべく少ない説明変数でモデルを組み立てることが推奨されており, 
その最低限必要な説明変数を探すために
[**モデル選択**](https://logics-of-blue.com/%E3%83%A2%E3%83%87%E3%83%AB%E9%81%B8%E6%8A%9E_%E5%AE%9F%E8%B7%B5%E7%B7%A8/)
という過程を介します. 
このほか, 
[多重共線性](https://best-biostatistics.com/correlation_regression/multi-co.html)や
[疑似相関](https://www.itpassportsiken.com/word/%E6%93%AC%E4%BC%BC%E7%9B%B8%E9%96%A2.html)
という言葉も知っておいた方が良いと思います.

<br>

## <span>02</span>**実践編(ポアソン分布)**

<br>

```{r stat040, fig.cap = CAP03, out.width=500}
CAP03 = "指何本分？"
knitr::include_graphics(DIR |> str_subset("modeling_004"))
```

<br>

研究室のサーバにいくつかの練習用データを用意しました.
たとえば, 
[釣り具のポイントのHP](https://www.point-i.jp/fishing_spot_guides?&free_word=%E3%82%BF%E3%83%81%E3%82%A6%E3%82%AA&page=1&prefecture_id=42)
に掲載されている釣果情報から, 長崎県内のタチウオの釣果だけを抽出したものがあるので, それを使用してみます.

<div class="ac-box">
<input id="ac-3" name="accordion-3" type="checkbox" />
<label for="ac-3"> Rのコードを確認する </label>
<div class="ac-small"><p>

```{r stat04, echo=T, eval=F}
# データの読み込み.
df = read_rds("~/Lab_Data/tanimaes/share_files/example_data/Tachiuo_fishing_point.rds") |>
  mutate(day = day(date)) |> 
  mutate(md = month + day/30) |> 
  drop_na(size)

# データの確認.
df

# 作図.
df |> 
  ggplot() +
  geom_point(aes(x = md, y = size, color = location), size = 2) +
  scale_x_continuous(breaks = 1:12,
                     limits = c(1,12),
                     name = "月") +
  scale_y_continuous(limits = c(0,8), 
                     name = "指何本分か") +
  scale_color_viridis_d(end = 0.9) +
  ggpubr::theme_pubr() +
  theme(legend.position = "right")
```

</p></div></div>

```{r stat05, echo=F, eval=T}
read_rds("~/Lab_Data/tanimaes/share_files/example_data/Tachiuo_fishing_point.rds") |>
  mutate(day = day(date)) |> 
  mutate(md = month + day/30) |> 
  drop_na(size)

read_rds("~/Lab_Data/tanimaes/share_files/example_data/Tachiuo_fishing_point.rds") |>
  mutate(day = day(date)) |> 
  mutate(md = month + day/30) |> 
  drop_na(size) |> 
  ggplot() +
  geom_point(aes(x = md, y = size, color = location), size = 4) +
  scale_x_continuous(breaks = 1:12,
                     limits = c(1,12),
                     name = "月") +
  scale_y_continuous(limits = c(0,8), 
                     name = "指何本分か") +
  scale_color_viridis_d(end = 0.9) +
  ggpubr::theme_pubr() +
  theme(legend.position = "right")

```

<br>

タチウオのサイズには, なんとなく季節的な傾向があることが分かります.
地点が多くて見にくいので, 船釣りか陸っぱりかで色分けしてみます.

<div class="ac-box">
<input id="ac-4" name="accordion-4" type="checkbox" />
<label for="ac-4"> Rのコードを確認する </label>
<div class="ac-small"><p>

```{r stat06, echo=T, eval=F}
# location に「沖」が含まれているものは「船釣り」とした.
df = df |> 
  mutate(place = ifelse(str_detect(location, pattern = "沖"), "船釣り", "陸っぱり"))

# 作図.
df|> 
  ggplot() +
  geom_point(aes(x = md, y = size, color = place), size = 4) +
  scale_x_continuous(breaks = 1:12,
                     limits = c(1,12),
                     name = "月") +
  scale_y_continuous(limits = c(0,8), 
                     name = "指何本分か") +
  scale_color_viridis_d(end = 0.6) +
  ggpubr::theme_pubr() +
  theme(legend.position = "right")

```

</p></div></div>

```{r stat07, echo=F, eval=T}
read_rds("~/Lab_Data/tanimaes/share_files/example_data/Tachiuo_fishing_point.rds") |>
  mutate(day = day(date)) |> 
  mutate(md = month + day/30) |> 
  drop_na(size) |> 
  mutate(place = ifelse(str_detect(location, pattern = "沖"), "船釣り", "陸っぱり")) |> 
  ggplot() +
  geom_point(aes(x = md, y = size, color = place), size = 4) +
  scale_x_continuous(breaks = 1:12,
                     limits = c(1,12),
                     name = "月") +
  scale_y_continuous(limits = c(0,8), 
                     name = "指何本分か") +
  scale_color_viridis_d(end = 0.6) +
  ggpubr::theme_pubr() +
  theme(legend.position = "right")

```

<br>

さらに傾向が見えてきました. 
x 軸を「月」から「気温」に変えられないか試してみます.
気温のデータは, 今のところ新上五島町有川のデータしかありませんが, 
県内での気温差はそれほど大きくないと思うので, 今回は有川の気温を使用してみます.

<div class="ac-box">
<input id="ac-5" name="accordion-5" type="checkbox" />
<label for="ac-5"> Rのコードを確認する </label>
<div class="ac-small"><p>

```{r stat08, echo=T, eval=F}
# 有川の気象データ（気象庁のHPのデータ）.
jma = read_rds("~/Lab_Data/tanimaes/seaweed_data/info/arikawa_JMA.rds")

# 日ごとに, 平均気温, 平均風速, 降水量を算出.
jma1 = jma |>
  mutate(date = as.Date(datetime)) |> 
  group_by(date) |> 
  summarise(mean_temp = mean(temp_air),
            mean_wind = mean(wind),
            sum_rain = sum(rain))

# データの結合.
df = df |> left_join(jma1, by = "date") |> drop_na()

# 作図.
df |> 
  ggplot() +
  geom_point(aes(x = mean_temp, y = size, color = place), size = 4) +
  scale_x_continuous(name = "日平均気温（℃）") +
  scale_y_continuous(limits = c(0,8), 
                     name = "指何本分か") +
  scale_color_viridis_d(end = 0.6) +
  ggpubr::theme_pubr() +
  theme(legend.position = "right")

```

</p></div></div>

```{r stat09, echo=F, eval=T}
stat09 = read_rds("~/Lab_Data/tanimaes/share_files/example_data/Tachiuo_fishing_point.rds") |>
  mutate(day = day(date)) |> 
  mutate(md = month + day/30) |> 
  drop_na(size) |> 
  mutate(place = ifelse(str_detect(location, pattern = "沖"), "船釣り", "陸っぱり"))

jma = read_rds("~/Lab_Data/tanimaes/seaweed_data/info/arikawa_JMA.rds") |> 
  mutate(date = as.Date(datetime)) |> 
  group_by(date) |> 
  summarise(mean_temp = mean(temp_air),
            mean_wind = mean(wind),
            sum_rain = sum(rain))

stat10 = stat09 |> 
  left_join(jma, by = "date") |> 
  drop_na()

stat10 |> 
  ggplot() +
  geom_point(aes(x = mean_temp, y = size, color = place), size = 4) +
  scale_x_continuous(name = "日平均気温（℃）") +
  scale_y_continuous(limits = c(0,8), 
                     name = "指何本分か") +
  scale_color_viridis_d(end = 0.6) +
  ggpubr::theme_pubr() +
  theme(legend.position = "right")

```

<br>

日平均気温が高い方が, タチウオのサイズが大きくなっていることが分かりました.
これはおそらくタチウオの生活史的な問題を含むため, 
水温が大きいとタチウオのサイズが大きくなるとは限りませんが,
季節的な要素とタチウオのサイズを照らし合わせる場合に, 気温は良い指標かもしれません.
そして, おそらく気温ではなく水温のデータを用いるべきです.

<br>

<h4 class="heading02">**いよいよ統計解析です**</h4>

<br>

収拾されたデータをよくみると,以下のことが分かります.

**・タチウオのサイズ: 正の整数**

**・気温：正の実数**

タチウオのサイズは本来なら連続型の変数ですが, 
釣り人に特有の測定方法（指何本分）が影響し, 整数しかとらないデータとなっています.
いいかどうかはわかりませんが, ここではとりあえずタチウオのサイズは離散型であると仮定して解析してみます.

<div class="ac-box">
<input id="ac-6" name="accordion-6" type="checkbox" />
<label for="ac-6"> Rのコードを確認する </label>
<div class="ac-small"><p>

```{r stat10, echo=T, eval=F}
df1 = df |> 
  mutate(place = as.factor(place))

# 一般化線形モデル.
m1 = glm(size ~ mean_temp + place, data = df1, family = poisson)

valuehat = predict(m1)
valuese = predict(m1, se.fit = T)$se.fit
dset = df1 |> 
  mutate(valuehat,
         valuese) |> 
  mutate(l95 = valuehat - 1.96 * valuese,
         u95 = valuehat + 1.96 * valuese) |> 
  mutate(across(c(valuehat, l95, u95),
                exp))

dset |> 
  ggplot(aes(x = mean_temp, group = place)) +
  geom_ribbon(aes(ymin = l95, ymax = u95, fill = place),
              alpha = 0.2) +
  geom_point(aes(y = size, color = place), size = 4) +
  geom_line(aes(y = valuehat, color = place), size = 2) +
  scale_color_viridis_d(end = 0.6) +
  scale_fill_viridis_d(end = 0.6) +
  scale_x_continuous(name = "日平均気温（℃）") +
  scale_y_continuous(limits = c(0,8), 
                     name = "指何本分か") +
  ggpubr::theme_pubr() +
  theme(legend.position = "right")

```

</p></div></div>

```{r stat11, echo=F, eval=T}
stat10 = stat10 |> mutate(place = as.factor(place))

# 一般化線形モデル.
m1 = glm(size ~ mean_temp + place, data = stat10, family = poisson)

dset = stat10 |> 
  mutate(valuehat = predict(m1),
         valuese = predict(m1, se.fit = T)$se.fit) |> 
  mutate(l95 = valuehat - 1.96 * valuese,
         u95 = valuehat + 1.96 * valuese) |> 
  mutate(across(c(valuehat, l95, u95),
                exp))

dset |> 
  ggplot(aes(x = mean_temp, group = place)) +
  geom_ribbon(aes(ymin = l95, ymax = u95, fill = place),
              alpha = 0.2) +
  geom_point(aes(y = size, color = place), size = 4) +
  geom_line(aes(y = valuehat, color = place), size = 2) +
  scale_color_viridis_d(end = 0.6) +
  scale_fill_viridis_d(end = 0.6) +
  scale_x_continuous(name = "日平均気温（℃）") +
  scale_y_continuous(limits = c(0,8), 
                     name = "指何本分か") +
  ggpubr::theme_pubr() +
  theme(legend.position = "right")

```

<br>

頻度論的に, ポアソン分布の一般化線形モデルを推定しました.
陸からのタチウオ釣りでは, 気温の低い時期は指 3 本程度が平均的, 
気温の高い時期は指 4 本程度が平均的であることが分かりました.
今回はモデル選択について紹介しませんでしたが, 皆さんがやるときは実施してください.

<br>

<div class="box27">
<span class="box-title">
Plus α
</span>
<p>

<br>

スクレイピングによって, WEB 上のデータを取り出してみよう.
ただし, 集中的に WEB サイトにアクセスをかける行為は, **相手のサーバーに大きな負荷をかける**ことになるため,
マナーが問われます.
そこで, **Sys.sleep()** 関数に 10 を渡して, 10 秒に 1 アクセスとなるように設定しました.

<div class="ac-box">
<input id="ac-60" name="accordion-60" type="checkbox" />
<label for="ac-60"> Rのコードを確認する </label>
<div class="ac-small"><p>


```{r stat110, echo=T, eval=F}
library(tidyverse)
library(rvest)
library(xml2)

# 釣り具のポイントの HP から釣果を抽出する自作関数をつくる.

get_fish_point = function(key, page){
  
  Sys.sleep(10)
  URL = str_glue("https://www.point-i.jp/fishing_spot_guides?&free_word=",key,"&page=",page,"&prefecture_id=42")
  out = read_html(URL)
  
  out1 = out |> 
    html_nodes(xpath = '//*[@class="card__layout--fishing-item-outer"]/span') |> 
    xml_text() |> 
    as_tibble() |> 
    mutate(n = rep(c("sp_title","species_j","size_title","size"), 15)) |> 
    pivot_wider(names_from = n, values_from = value, values_fn = list) |>
    unnest() |> 
    select(species_j, size)
  
  out2 = out |>
    html_nodes(xpath = '//*/span[@class="card__date"]') |> 
    xml_text() |> 
    as_tibble() |> 
    mutate(value = str_remove_all(value, pattern = "[\\s, '\n']")) |> 
    mutate(value = as.Date(value)) |> 
    rename(date = value)
  
  out3 = out |>
    html_nodes(xpath = '//*/h1[@class="card__title card__title--fishing-spot"]') |> 
    xml_text() |> 
    as_tibble() |> 
    mutate(value = str_remove_all(value, pattern = "[\\s, '\n']")) |> 
    rename(location = value)
  
  bind_cols(out1, out2, out3)
  
}

# 以下のコードは慎重に走らせてください.
test = tibble(page = 1:7, key = "タチウオ") |>
  mutate(data = map2(key, page, get_fish_point)) |>
  select(data) |>
  unnest(data)

test

```

</p></div></div>

</details>
</p></div>

<br>

## <span>03</span>**実践編（ガンマ分布）**

<br>

```{r stat100, fig.cap = CAP04, out.width=500}
CAP04 = "二枚貝."
knitr::include_graphics(DIR |> str_subset("modeling_005"))
```

<br>

とある二枚貝の殻 1 枚ごとの計測データもサーバー内に保管しています.
ここでは, 貝殻の殻長と重量の関係性についても解析してみます.

**・殻長: 正の実数**

**・重量：正の実数**

説明変数を殻長, 応答変数を重量とします.
連続的な正の数値をとるため, ガンマ分布で解析してみます.
また, 累乗モデルも試します.
ガンマ分布のGLMと累乗モデルを AIC で比較します.

<div class="ac-box">
<input id="ac-7" name="accordion-7" type="checkbox" />
<label for="ac-7"> Rのコードを確認する </label>
<div class="ac-small"><p>

```{r  stat12, echo=T, eval=F}
df2 = readxl::read_xlsx("~/Labs_work_tanimae_2020/seashell/matsuyama.xlsx")

# model
m1 = glm(weight ~ width, data = df2, family = Gamma(link = "log"))
m2 = glm(log(weight) ~ log(width), data = df2)

AIC(m1, m2)

dset = df2 |> expand(width = seq(min(width), max(width), by = 1))

dset1 = dset |> 
  mutate(valuehat = predict(m1, newdata = dset),
         valuese = predict(m1, newdata = dset, se.fit = T)$se.fit) |> 
  mutate(l95 = valuehat - 1.96 * valuese,
         u95 = valuehat + 1.96 * valuese) |> 
  mutate(across(c(valuehat, l95, u95),
                exp))

dset2 = dset |> 
  mutate(valuehat = predict(m2, newdata = dset),
         valuese = predict(m2, newdata = dset, se.fit = T)$se.fit) |> 
  mutate(l95 = valuehat - 1.96 * valuese,
         u95 = valuehat + 1.96 * valuese) |> 
  mutate(across(c(valuehat, l95, u95),
                exp))

g1 = dset1 |> 
  ggplot() + 
  geom_line(aes(x = width, y = valuehat), 
            size = 2, color = "darkslategray4") +
  geom_ribbon(aes(x = width,  
                  ymin = l95,
                  ymax = u95),
              alpha = 0.3, fill = "darkslategray4") +
  geom_point(aes(x = width, y = weight), 
             data = df2, size = 4, shape =1) +
  ggtitle("ガンマ分布のGLM") +
  scale_x_continuous("Width (mm)") +
  scale_y_continuous("Weight (g)") +
  ggpubr::theme_pubr()

g2 = dset2 |> 
  ggplot() + 
  geom_line(aes(x = width, y = valuehat), 
            size = 2, color = "darkslategray4") +
  geom_ribbon(aes(x = width,  
                  ymin = l95,
                  ymax = u95),
              alpha = 0.3, fill = "darkslategray4") +
  geom_point(aes(x = width, y = weight), 
             data = df2, size = 4, shape =1) +
  ggtitle("累乗モデル") +
  scale_x_continuous("Width (mm)") +
  scale_y_continuous("Weight (g)") +
  ggpubr::theme_pubr()

library(patchwork)

g1+g2

```

</p></div></div>

```{r stat13, echo=F, eval=T}
df2 = readxl::read_xlsx("~/Labs_work_tanimae_2020/seashell/matsuyama.xlsx")

# model
m1 = glm(weight ~ width, data = df2, family = Gamma(link = "log"))
m2 = glm(log(weight) ~ log(width), data = df2)

AIC(m1, m2)

dset = df2 |> expand(width = seq(min(width), max(width), by = 1))

dset1 = dset |> 
  mutate(valuehat = predict(m1, newdata = dset),
         valuese = predict(m1, newdata = dset, se.fit = T)$se.fit) |> 
  mutate(l95 = valuehat - 1.96 * valuese,
         u95 = valuehat + 1.96 * valuese) |> 
  mutate(across(c(valuehat, l95, u95),
                exp))

dset2 = dset |> 
  mutate(valuehat = predict(m2, newdata = dset),
         valuese = predict(m2, newdata = dset, se.fit = T)$se.fit) |> 
  mutate(l95 = valuehat - 1.96 * valuese,
         u95 = valuehat + 1.96 * valuese) |> 
  mutate(across(c(valuehat, l95, u95),
                exp))

g1 = dset1 |> 
  ggplot() + 
  geom_line(aes(x = width, y = valuehat), 
            size = 2, color = "darkslategray4") +
  geom_ribbon(aes(x = width,  
                  ymin = l95,
                  ymax = u95),
              alpha = 0.3, fill = "darkslategray4") +
  geom_point(aes(x = width, y = weight), 
             data = df2, size = 4, shape =1) +
  ggtitle("ガンマ分布のGLM") +
  scale_x_continuous("Width (mm)") +
  scale_y_continuous("Weight (g)") +
  ggpubr::theme_pubr()

g2 = dset2 |> 
  ggplot() + 
  geom_line(aes(x = width, y = valuehat), 
            size = 2, color = "darkslategray4") +
  geom_ribbon(aes(x = width,  
                  ymin = l95,
                  ymax = u95),
              alpha = 0.3, fill = "darkslategray4") +
  geom_point(aes(x = width, y = weight), 
             data = df2,size = 4, shape =1) +
  ggtitle("累乗モデル") +
  scale_x_continuous("Width (mm)") +
  scale_y_continuous("Weight (g)") +
  ggpubr::theme_pubr()

library(patchwork)

g1+g2
```

AIC は累乗モデルの方が低くなっています.
モデルの当てはまりも累乗モデルの方が良いような気がします.
この結果からもわかるように, 推定されたモデルについては残差の確認が必要です.
でも何でガンマの方が当てはまりが悪いんだろう？

<br>

## <span>04</span>**実践編（二項分布）**

<br>

新型コロナウイルスの感染者数や死亡者数のデータはネット上にたくさん転がっています.
NHKのHPに使い勝手の良いものがあるので, 引っ張ってきてみます.

<div class="ac-box">
<input id="ac-8" name="accordion-8" type="checkbox" />
<label for="ac-8"> Rのコードを確認する </label>
<div class="ac-small"><p>

```{r stat14, echo=T, eval=F}
# covid-19 data from HNK. 
covid = read_csv("https://www3.nhk.or.jp/n-data/opendata/coronavirus/nhk_news_covid19_prefectures_daily_data.csv", 
                 col_names = c("date", "prefcode", "pref", "new_cases", "new_cases_cs", "deaths", "deaths_cs"),
                 skip = 1,
                 show_col_types = F) |> 
  mutate(date = as.Date(date),
         prefcode = as.double(prefcode))

covid |> 
  ggplot() +
  geom_line(aes(date, new_cases, color = pref)) +
  scale_color_viridis_d(end = 0.9) +
  ggpubr::theme_pubr() +
  theme(legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.background = element_blank(),
        legend.title = element_blank())

```

</p></div></div>

```{r stat15, echo=F, eval=T}
covid = read_csv("https://www3.nhk.or.jp/n-data/opendata/coronavirus/nhk_news_covid19_prefectures_daily_data.csv", 
                 col_names = c("date", "prefcode", "pref", "new_cases", "new_cases_cs", "deaths", "deaths_cs"),
                 skip = 1,
                 show_col_types = F) |> 
  mutate(date = as.Date(date),
         prefcode = as.double(prefcode))

covid |> 
  ggplot() +
  geom_line(aes(date, new_cases, color = pref)) +
  scale_color_viridis_d(end = 0.9) +
  ggpubr::theme_pubr() +
  theme(legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.background = element_blank(),
        legend.title = element_blank())
```

<br>

1日ごとに, 新規感染者数や死亡者数が記録されていることが分かります.
今度は, 少しデータを整形して, **1 人目の感染者の有無**と**1 人目の死亡者の有無**
について, **有り**の時は **1**, **無し**のときは **0** のデータとして調べてみます.
ただしデータが多すぎるので, 30 日につき 1 日だけのデータを抽出しようと思います.

そしてここでも, 解析まで一気にやってみます.
0, 1 のデータなので二項分布の一般化線形モデルを記述します.

<br>

<div class="ac-box">
<input id="ac-9" name="accordion-9" type="checkbox" />
<label for="ac-9"> Rのコードを確認する </label>
<div class="ac-small"><p>

```{r stat16, echo=T, eval=F}
covid = covid |> 
  select(date, pref, new_cases, new_cases_cs, deaths, deaths_cs) |> 
  group_by(pref) |> 
  mutate(n = row_number()/30) |>
  ungroup() |> 
  filter(n %% 1 == 0)

covid = covid |> 
  group_by(pref) |> 
  mutate(cases_tf = ifelse(new_cases_cs > 0, 1, 0),
         deaths_tf = ifelse(deaths_cs > 0, 1, 0)) |> 
  ungroup() |> 
  select(date, pref, cases_tf, deaths_tf) |> 
  pivot_longer(cols = c(cases_tf, deaths_tf), names_to = "type", values_to = "value") |> 
  mutate(type = recode(type, cases_tf = "1 人目の感染者の有無"),
         type = recode(type, deaths_tf = "1 人目の死亡者の有無")) |> 
  mutate(type = as.factor(type))

# 一般化線形モデル.
m1 = glm(value ~ date * type, data = covid, family = binomial(link = "logit"))

pdata = covid |> expand(date = seq(min(date), max(date), by = 7), type = type)

pdata = pdata |> 
  bind_cols(predict(m1, newdata = pdata, type = "response", se.fit = T) |> 
              as_tibble()) |> 
  mutate(l95 = fit - 1.96 * se.fit,
         u95 = fit + 1.96 * se.fit)

pdata |> 
  ggplot(aes(x = date))+
  geom_ribbon(aes(ymin = l95, ymax = u95, group = type, fill = type),
              alpha = 0.3) +
  geom_jitter(aes(x = date, y = value, color = type), 
              data = covid, size = 0.5,alpha = 0.4, width = 10, height = 0.03, shape = 1) +
  geom_line(aes(y = fit, group = type, color = type), size = 1) +
  scale_color_viridis_d(end = 0.6) +
  scale_fill_viridis_d(end = 0.6) +
  scale_y_continuous(breaks = c(0, 1),
                     labels = c("無し" ,"有り")) +
  facet_grid(type ~ .) +
  ggpubr::theme_pubr() +
  theme(axis.title = element_blank(),
        legend.position = "none")
```

</p></div></div>

```{r stat17, echo=F, eval=T}
covid = read_csv("https://www3.nhk.or.jp/n-data/opendata/coronavirus/nhk_news_covid19_prefectures_daily_data.csv", 
                 col_names = c("date", "prefcode", "pref", "new_cases", "new_cases_cs", "deaths", "deaths_cs"),
                 skip = 1,
                 show_col_types = F) |> 
  mutate(date = as.Date(date),
         prefcode = as.double(prefcode))

covid = covid |> 
  select(date, pref, new_cases, new_cases_cs, deaths, deaths_cs) |> 
  group_by(pref) |> 
  mutate(n = row_number()/30) |>
  ungroup() |> 
  filter(n %% 1 == 0)

covid = covid |> 
  group_by(pref) |> 
  mutate(cases_tf = ifelse(new_cases_cs > 0, 1, 0),
         deaths_tf = ifelse(deaths_cs > 0, 1, 0)) |> 
  ungroup() |> 
  select(date, pref, cases_tf, deaths_tf) |> 
  pivot_longer(cols = c(cases_tf, deaths_tf), names_to = "type", values_to = "value") |> 
  mutate(type = recode(type, cases_tf = "1 人目の感染者の有無"),
         type = recode(type, deaths_tf = "1 人目の死亡者の有無")) |> 
  mutate(type = as.factor(type))

# 一般化線形モデル.
m1 = glm(value ~ date * type, data = covid, family = binomial(link = "logit"))

pdata = covid |> expand(date = seq(min(date), max(date), by = 7), type = type)

pdata = pdata |> 
  bind_cols(predict(m1, newdata = pdata, type = "response", se.fit = T) |> 
              as_tibble()) |> 
  mutate(l95 = fit - 1.96 * se.fit,
         u95 = fit + 1.96 * se.fit)

pdata |> 
  ggplot(aes(x = date))+
  geom_ribbon(aes(ymin = l95, ymax = u95, group = type, fill = type),
              alpha = 0.3) +
  geom_jitter(aes(x = date, y = value, color = type), 
              data = covid, size = 0.5,alpha = 0.4, width = 10, height = 0.03, shape = 1) +
  geom_line(aes(y = fit, group = type, color = type), size = 1) +
  scale_color_viridis_d(end = 0.6) +
  scale_fill_viridis_d(end = 0.6) +
  scale_y_continuous(breaks = c(0, 1),
                     labels = c("無し" ,"有り")) +
  facet_grid(type ~ .) +
  ggpubr::theme_pubr() +
  theme(axis.title = element_blank(),
        legend.position = "none")

```

<br>

全都道府県のうち, どれくらいの割合で新型コロナウイルスによる蝕みが進行したか, を解析することが出来ました.
ここでは, 1 人目の感染者の有無と, 1 人目死亡者の有無に交互作用を持たせた GLM としました.

<br>

## <span>05</span>**実践編（ノンパラメトリックな方法）**

<br>

```{r stat181, fig.cap = CAP05, out.width=500}
CAP05 = "有川で見つけたムラサキダコ."
knitr::include_graphics(DIR |> str_subset("modeling_006"))
```

<br>

ネット上からムラサキダコの出現記録を収集し, エクセルに書き出したものがあるので,
ここではそのファイルを使用します.

<div class="ac-box">
<input id="ac-10" name="accordion-10" type="checkbox" />
<label for="ac-10"> Rのコードを確認する </label>
<div class="ac-small"><p>

```{r stat18, echo=T, eval=F}
# フォルダ内部のファイル名を見るためには dir() 関数を使います.
files = dir("~/Lab_Data/tanimaes/share_files/example_data/", # ディレクトリ
            full.names = T ) # 絶対パス (フルパス) を取り出す.

# ムラサキダコの出現記録のデータの読み出し.
tako = files |>  
  str_subset("Tremoctopus_occurrence_data.xlsx") |> 
  readxl::read_xlsx()

tako
```

</p></div></div>

```{r stat19, echo=F, eval=T}
dir("~/Lab_Data/tanimaes/share_files/example_data/", full.names = T) |>  
  str_subset("Tremoctopus_occurrence_data.xlsx") |> 
  readxl::read_xlsx()
```

<br>

データの列は, **種名**, **経度**, **緯度**, **日付**, **情報源** の順番に並んでいます.
ここから, x 軸に月, y 軸にムラサキダコの出現数を取ったヒストグラムを作成してみます.

<div class="ac-box">
<input id="ac-11" name="accordion-11" type="checkbox" />
<label for="ac-11"> Rのコードを確認する </label>
<div class="ac-small"><p>

```{r stat20, echo=T, eval=F}
tako = tako |> 
  mutate(date = ymd(date)) |> 
  mutate(month = month(date)) 

tako |> 
  ggplot() +
  geom_histogram(aes(x = month), stat = "count") +
  scale_x_continuous(breaks = 1:12)

```

</p></div></div>

```{r stat21, echo=F, eval=T}
dir("~/Lab_Data/tanimaes/share_files/example_data/", full.names = T) |>  
  str_subset("Tremoctopus_occurrence_data.xlsx") |> 
  readxl::read_xlsx() |> 
  mutate(date = ymd(date)) |> 
  mutate(month = month(date),
         year = year(date)) |>  
  ggplot() +
  geom_histogram(aes(x = month), stat = "count") +
  scale_x_continuous(breaks = 1:12) 
```

<br>

ヒストグラムをみてみると, 外洋性・遊泳性の頭足類であるムラサキダコは, 
おおよそ晩夏以降に日本の沿岸に接岸していることが分かります.

ここで, 年毎にグループ化し, 月毎の観測割合を算出します.
そして, 観測割合を月で説明した一般化加法モデルを記述し, 解析してみます.
スプラインにはいろいろな種類がありますが, x 軸は月なので, 12 月の次は 1 月に戻ることを考慮し,
周期的 3 次回帰スプライン平滑法 (Cyclic cubic regression splines smoothing) を使用しました.
これは, 両端点の 2 次微分までの値が等しくなるという制約のある 3 次回帰スプライン平滑法です。

<br>

<div class="ac-box">
<input id="ac-12" name="accordion-12" type="checkbox" />
<label for="ac-12"> Rのコードを確認する </label>
<div class="ac-small"><p>

```{r stat22, echo=T, eval=F}
tako1 = tako |> 
  mutate(year = year(date)) |> 
  group_by(year, month) |> 
  summarise(n=n(), .groups = "drop") |> 
  group_by(year) |> 
  mutate(scaled_n = n/sum(n)) |> # 年でグループ化し, 月ごとの観測割合を算出. 
  ungroup()

library(mgcv)

# cyclic cubic regression splines.
m0 = gam(scaled_n ~ s(month, bs = "cc", k = 4), data = tako1, family = gaussian(link = "log"))
# cubic spline basis defined by a modest sized set of knots spread evenly through the covariate values. 
m1 = gam(scaled_n ~ s(month, bs = "cr", k = 4), data = tako1, family = gaussian(link = "log"))

AIC(m0, m1) # 検討したモデルの中から一番低いのを選ぶ.

summary(m0) # model summary

# 期待値と信頼区間の算出.
dset = tako1 |> expand(month = seq(min(month), max(month), by = 0.05))

pdata1 = dset |> 
  bind_cols(predict(m0, 
                    newdata = dset,
                    interval = 'confidence',
                    lvel = 0.95,
                    se.fit = T) |>
              as_tibble()) |> 
  mutate(l95 = fit - 1.96 * se.fit,
         u95 = fit + 1.96 * se.fit) |> 
  mutate(across(c(fit, l95, u95), exp))

ggplot() + 
  geom_ribbon(aes(x = month, ymin = l95, ymax = u95),
              data = pdata1, fill = "turquoise4", alpha = 0.3)+
  geom_line(aes(x = month, y = fit),
            data = pdata1, color = "turquoise4",linetype = "dashed", size = 2) +
  geom_jitter(aes(x = month, y = scaled_n), 
              data = tako1, size = 3, width = 0.15) +
  scale_x_continuous(breaks = 1:12, name = "月") +
  scale_y_continuous(breaks = c(0, 0.5, 1), name = "出現確率") +
  ggpubr::theme_pubr()
```

</p></div></div>

```{r stat23, echo=F, eval=T}
tako1 = dir("~/Lab_Data/tanimaes/share_files/example_data/", full.names = T) |>  
  str_subset("Tremoctopus_occurrence_data.xlsx") |> 
  readxl::read_xlsx() |> 
  mutate(date = ymd(date)) |> 
  mutate(month = month(date),
         year = year(date)) |> 
  group_by(year, month) |> 
  summarise(n=n()) |> 
  mutate(scaled_n = n/sum(n)) |> # 年でグループ化し, 月ごとの観測割合を算出. 
  ungroup()

library(mgcv)

# cyclic cubic regression splines.
m0 = gam(scaled_n ~ s(month, bs = "cc", k = 4), data = tako1, family = gaussian(link = "log"))
# cubic spline basis defined by a modest sized set of knots spread evenly through the covariate values. 
# m1 = gam(scaled_n ~ s(month, bs = "cr", k = 4), data = tako1, family = gaussian(link = "log"))

# summary(m0) # model summary

# 期待値と信頼区間の算出.
dset = tako1 |> expand(month = seq(min(month), max(month), by = 0.05))

pdata1 = dset |> 
  bind_cols(predict(m0, 
                    newdata = dset,
                    interval = 'confidence',
                    lvel = 0.95,
                    se.fit = T) |>
              as_tibble()) |> 
  mutate(l95 = fit - 1.96 * se.fit,
         u95 = fit + 1.96 * se.fit) |> 
  mutate(across(c(fit, l95, u95), exp))

ggplot() + 
  geom_ribbon(aes(x = month, ymin = l95, ymax = u95),
              data = pdata1, fill = "turquoise4", alpha = 0.3)+
  geom_line(aes(x = month, y = fit),
            data = pdata1, color = "turquoise4",linetype = "dashed", size = 1.5) +
  geom_jitter(aes(x = month, y = scaled_n), 
              data = tako1, size = 3, width = 0.15) +
  scale_x_continuous(breaks = 1:12, name = "月") +
  scale_y_continuous(breaks = c(0, 0.5, 1), name = "出現確率") +
  ggpubr::theme_pubr()
```

<br>

ムラサキダコは 8 ~ 9 月頃に沿岸域での報告数が多くなっていることが分かります.

<br><br><br>
