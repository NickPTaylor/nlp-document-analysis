# Advanced PDF Parsing {#ch:advanced-pdf-parsing}



## Introduction

### Objective

This report addresses the objective:

> Given a large PDF document with several chapters/sections, apply NLP (Natural Language Processing) techniques to each *section* in order to gain insight into the content of each section.

The document being considered for this example is [Shakespeare's Macbeth](https://www.williamshakespeare.net/pdf/macbeth.pdf) and the objective is to split the document into a corpus of sections.

### HTML Documents

Parsing an HTML (Hyper-text Markup Language) document i.e. a web page by section is relatively easy since the HTML markup language explicitly encodes the structure of the document i.e. HTML directly specifies whether each item of text is a heading, paragraph, list, etc.  Headings are typically enclosed by one of six HTML markup tags i.e. `<h1>`, `<h2>`, etc. wherein the number succeeding the 'h' reflects the hierarchical importance of the heading.  For example, `<h1>` tags may reflect a 'title' heading, `<h1>` a 'chapter', `<h2>` a section and so forth.  HTML only describes the *structure* of the document; the *formatting* of the document, describing how the text is rendered is (usually) described elsewhere e.g. a CSS (Cascading Style Sheet) file.

### PDF Documents

The raw encoding for a PDF document is fundamentally different to that of an HTML document.  Whereas the raw code for HTML contains the document content and explicit encoding of the document *structure*, a PDF document describes the content and the *formatting* e.g. font style, font size, character location, etc.  The structure is therefore implicit, rather than explicit and it is only possible to *infer* the structural aspects of the document from the code describing the formatting of the document.  For example, if an item of text is formatted to be rendered with a large font, it could be inferred that the text is a 'title' heading.  In this report, a method for parsing a PDF document will be described.  The method is facilitated by the `pdftools` package [@R-pdftools]^[The `pdftools` library requires the `poppler` development library.  Installation instructions are described [here](https://ropensci.org/blog/2016/03/01/pdftools-and-jeroen/).]. 

## Method

### Read Document

Using `pdftools::pdf_data` the document is read, word-by-word.  For later convenience, the document *line number* of each word is computed and included with the data.  The first and last words of extracted raw data are inspected:


```r
# Download document to temporary location.
destfile = tempfile('macbeth', fileext = '.pdf')
download.file(url = 'https://www.williamshakespeare.net/pdf/macbeth.pdf',
              destfile = destfile, quiet = TRUE)

# Read metadata.
df_doc_data_raw <- pdftools::pdf_data(destfile) %>%
    dplyr::bind_rows(.id = 'page') %>%
    dplyr::mutate(page = as.integer(page)) %>%
    dplyr::mutate(text = text %>%
        stringr::str_remove_all('[^[:print:]]+') %>%
        stringr::str_squish())

# Remove temporary file.
unlink(destfile)

# Compute line numbers.
line_no <- df_doc_data_raw %>%
    dplyr::distinct(page, y) %>%
    dplyr::arrange(page, y) %>%
    dplyr::mutate(doc_line = row_number())

df_doc_data_raw %<>%
    dplyr::left_join(line_no, by = c('page', 'y'))

# Print.
df_doc_data_raw %>% head()
df_doc_data_raw %>% tail()
```

```
## # A tibble: 6 x 8
##    page width height     x     y space text     doc_line
##   <int> <int>  <int> <int> <int> <lgl> <chr>       <int>
## 1     1    48     11   458    38 TRUE  HAMLET          1
## 2     1     3     11   510    38 TRUE  -               1
## 3     1    17     11   517    38 TRUE  Act             1
## 4     1     8     11   538    38 FALSE V               1
## 5     1     6     11   535   794 FALSE 1               2
## 6     2    58     15    54    81 FALSE Contents        3
## # A tibble: 6 x 8
##    page width height     x     y space text      doc_line
##   <int> <int>  <int> <int> <int> <lgl> <chr>        <int>
## 1    83    42     12   197   476 TRUE  crown'd       3360
## 2    83     9     12   244   476 TRUE  at            3360
## 3    83    36     12   257   476 FALSE Scone.        3360
## 4    83    50     12    54   505 TRUE  Flourish.     3361
## 5    83    37     12   108   505 FALSE Exeunt        3361
## 6    83    13     11   529   794 FALSE 83            3362
```

The extracted metadata is:

* `text`: the word
* `page`: the page that the word occurs on
* `width`: the width of the word i.e. font width
* `height`: the height of the word i.e. font height
* `x`: the horizontal location of the word on the page
* `y`: the vertical location of the word on the page
* `space`: whether the word should have a space appended

The line number, `doc_line`, is computed from the raw metadata by identifying unique combinations of `page` and `y`.  As stated above, the word metadata demonstrates that the internal representation of a PDF combines the description of the *formatting* and the *content* and does not explicitly describe the *structure*.

### Identify Headings

It may be possible to extract further metadata relating to each word such as font type; however, this has not been explored presently.  For the method described herein, the word *height* is used to identify candidate words that *may* be section heading content.  Some exploration indicated that words that are of height 22 are likely to be section headings for the document being analysed:


```r
df_doc_data_raw %>%
    dplyr::filter(height == HEADING_SIZE)
```

```
## # A tibble: 10 x 8
##     page width height     x     y space text  doc_line
##    <int> <int>  <int> <int> <int> <lgl> <chr>    <int>
##  1     3    47     22    54    56 TRUE  ACT         38
##  2     3     6     22   108    56 FALSE I           38
##  3    20    47     22    54   116 TRUE  ACT        751
##  4    20    13     22   108   116 FALSE II         751
##  5    34    47     22    54   239 TRUE  ACT       1322
##  6    34    20     22   108   239 FALSE III       1322
##  7    51    47     22    54    71 TRUE  ACT       2019
##  8    51    22     22   108    71 FALSE IV        2019
##  9    69    47     22    54   229 TRUE  ACT       2779
## 10    69    16     22   108   229 FALSE V         2779
```

The raw data frame contains a row for each word.  The objective of the following code is to traverse the data frame above and derive groups of words that form each individual section heading.  This is achieved by first filtering words that have the section heading height.   Subsequently, by joining these filtered words that either occur on the same line or on consecutive lines, section headings can be inferred.  It is necessary to consider consecutive lines since it is possible that long section headings contain a line break although this does not occur in the current document.

The procedure in the code block below is:

* Filter words with a height matching the section heading height.
* Identify whether the words in the next row(s) are also part of the same heading i.e. on the same or consecutive lines in the document.
* Assign a unique ID to words belonging to the same heading.
* Concatenate words belonging to the same heading.


```r
# Derive headings.
df_headings <- df_doc_data_raw %>%
    # Filter words that make up headings.
    dplyr::filter(height == HEADING_SIZE) %>%
    # Group words that form each heading and assign head_ID.
    dplyr::mutate(doc_line_diff = doc_line - lag(doc_line)) %>%
    tidyr::replace_na(list(doc_line_diff = -1)) %>%
    dplyr::mutate(head_id = cumsum(doc_line_diff > 1)) %>%
    # Concatenate words with the same head_ID.
    dplyr::group_by(head_id) %>%
    dplyr::summarise(doc_line = first(doc_line),
                     page = first(page),
                     heading = str_c(text, collapse = " "), .groups = 'drop')

df_headings
```

```
## # A tibble: 5 x 4
##   head_id doc_line  page heading
##     <int>    <int> <int> <chr>  
## 1       0       38     3 ACT I  
## 2       1      751    20 ACT II 
## 3       2     1322    34 ACT III
## 4       3     2019    51 ACT IV 
## 5       4     2779    69 ACT V
```

The reader may refer to the [imported document](https://www.williamshakespeare.net/pdf/macbeth.pdf), to confirm that the section headings are correctly captured here.  The section heading words can now be joined to the raw data.  The procedure in the following code chunk is:

* Select the required metadata.
* Join the section headings derived above to the first word of each section in the raw data.
* Fill the section headings forwards so that the text between heading A and heading B is assigned to heading A.

Note that the last page (of the last section) should be assigned otherwise the last the section will be assumed to continue until the end of the text.


```r
# Assign headings to raw data.
df_doc_data <- df_doc_data_raw %>%
    dplyr::select(page, doc_line, text) %>%
    dplyr::filter(page <= LAST_PAGE) %>%
    # Merge headings.
    dplyr::left_join(df_headings, by = c('page', 'doc_line')) %>%
    # Fill headings forward
    tidyr::fill(heading, head_id) %>%
    tidyr::drop_na()
```

### Corpus by Heading

Finally, the data frame is grouped by section and the words within the section are concatenated to form the document corpus.


```r
df_corpus <- df_doc_data %>%
    dplyr::group_by(heading) %>%
    summarise(corpus = text %>%
        str_c(collapse = " ") %>%
        stringr::str_trunc(500, 'center') %>%
        stringr::str_wrap(), .groups = 'drop')

cat(df_corpus$corpus, sep = '\n--------\n')
```

```
## ACT I SCENE I. A desert place. Thunder and lightning. Enter three Witches First
## Witch When shall we three meet again In thunder, lightning, or in rain? Second
## Witch When the hurlyburly's done, When the battle's lost and won. Third Witch
## That will be...her, As we shall make our griefs and clamour roar Upon his death?
## MACBETH I am settled, and bend up Each corporal agent to this terrible feat. 19
## Away, and mock the time with fairest show: False face must hide what the false
## heart doth know. Exeunt
## --------
## ACT II SCENE I. Court of Macbeth's castle. Enter BANQUO, and FLEANCE bearing
## a torch before him BANQUO How goes the night, boy? FLEANCE The moon is down; I
## have not heard the clock. BANQUO And she goes down at twelve. FLEANCE I take't,
## 'tis later, s..., I will thither. MACDUFF Well, may you see things well done
## there: adieu! Lest our old robes sit easier than our new! ROSS Farewell, father.
## Old Man God's benison go with you; and with those That would make good of bad,
## and friends of foes! Exeunt
## --------
## ACT III SCENE I. Forres. The palace. Enter BANQUO BANQUO Thou hast it now: king,
## Cawdor, Glamis, all, As the weird women promised, and, I fear, Thou play'dst
## most foully for't: yet it was said It should not stand in thy posterity, But
## that myself sh...istance His wisdom can provide. Some holy angel Fly to the
## court of England and unfold His message ere he come, that a swift blessing May
## soon return to this our suffering country Under a hand accursed! Lord I'll send
## my prayers with him. 50 Exeunt
## --------
## ACT IV SCENE I. A cavern. In the middle, a boiling cauldron. Thunder. Enter the
## three Witches First Witch Thrice the brinded cat hath mew'd. Second Witch Thrice
## and once the hedge-pig whined. Third Witch Harpier cries 'Tis time, 'tis time.
## First Wit...e goes manly. Come, go we to the king; our power is ready; Our lack
## is nothing but our leave; Macbeth Is ripe for shaking, and the powers above Put
## on their instruments. Receive what cheer you may: The night is long that never
## finds the day. Exeunt
## --------
## ACT V SCENE I. Dunsinane. Ante-room in the castle. Enter a Doctor of Physic
## and a Waiting-Gentlewoman Doctor I have two nights watched with you, but can
## perceive no truth in your report. When was it she last walked? Gentlewoman
## Since his majesty wen...nt hands Took off her life; this, and what needful else
## That calls upon us, by the grace of Grace, We will perform in measure, time
## and place: So, thanks to all at once and to each one, Whom we invite to see us
## crown'd at Scone. Flourish. Exeunt 83
```

The data is now in a form whereby NLP (Natural Language Processing) methods can by applied by section.

## Conclusion

It is possible to infer PDF section headings by manipulating the metadata available from the PDF encoding.  It is considerably more challenging to traverse the section hierarchy of PDF documents than HTML documents.  The method described herein requires assumptions and interpretations in order to infer what text is a section heading and what the hierarchy is.  In contrast, markup tags make this explicit in HTML documents.  For section-by-section processing of a PDF document, human inspection and interpretation would be required for each document of study i.e. to determine likely section heading heights.
