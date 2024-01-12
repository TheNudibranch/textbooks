html_attributes <- function(list) {
  if (length(list) == 0) return("")
  
  attr <- map2_chr(names(list), list, html_attribute)
  paste0(" ", unlist(attr), collapse = "")
}
html_attribute <- function(name, value = NULL) {
  if (length(value) == 0) return(name) # for attributes with no value
  if (length(value) != 1) stop("`value` must be NULL or length 1")
  
  if (is.logical(value)) {
    # Convert T and F to true and false
    value <- tolower(value)
  } else {
    value <- escape_attr(value)
  }
  paste0(name, "='", value, "'")
}
escape_attr <- function(x) {
  x <- escape.character(x)
  x <- gsub("\'", '&#39;', x)
  x <- gsub("\"", '&quot;', x)
  x <- gsub("\r", '&#13;', x)
  x <- gsub("\n", '&#10;', x)
  x
}

tags <- c("a", "abbr", "address", "article", "aside", "audio",
          "b","bdi", "bdo", "blockquote", "body", "button", "canvas",
          "caption","cite", "code", "colgroup", "data", "datalist",
          "dd", "del","details", "dfn", "div", "dl", "dt", "em",
          "eventsource","fieldset", "figcaption", "figure", "footer",
          "form", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header",
          "hgroup", "html", "i","iframe", "ins", "kbd", "label",
          "legend", "li", "mark", "map","menu", "meter", "nav",
          "noscript", "object", "ol", "optgroup", "option", "output",
          "p", "pre", "progress", "q", "ruby", "rp","rt", "s", "samp",
          "script", "section", "select", "small", "span", "strong",
          "style", "sub", "summary", "sup", "table", "tbody", "td",
          "textarea", "tfoot", "th", "thead", "time", "title", "tr",
          "u", "ul", "var", "video"
)

void_tags <- c("area", "base", "br", "col", "command", "embed",
               "hr", "img", "input", "keygen", "link", "meta", "param",
               "source", "track", "wbr"
)