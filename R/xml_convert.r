#' @title Package 'xmlconvert'
#'
#' @description Tools for converting XML documents to dataframes and vice versa
#'
#' Functions available:
#' \itemize{
#' \item \code{\link{xml_to_df}()}: Converts an XML document to an R dataframe
#' \item \code{\link{df_to_xml}()}: Converts a dataframe to an XML document
#'}
#'
#'@name xmlconvert
NULL



ifnull <- function(x, alt) {
  ifelse(is.null(x), alt, x)
}


url_exists <- function(test.url) {
  res<-tryCatch({ invisible(httr::http_status(httr::GET(test.url))) }, error=function(e) { invisible(FALSE) })
  if(!is.logical(res)) {
    if(res$category == "Success") return(TRUE)
    else return(FALSE)
  }
  else return(FALSE)
}


escape <- function(char) {
  if(char %in% c(".","\\", "|", "(", ")", "[", "{", "^", "$", "*", "+", "?")) return(paste0("\\", char))
  else return(char)
}


drilldown <- function(node, hierarchy.field.delim, hierarchy.value.sep, no.hierarchy) {
  txt <- ""
  ch <- xml2::xml_children(node)
  if(length(ch) == 0) return(xml2::xml_text(node))
  else {
    if(!no.hierarchy) {
      if(NROW(hierarchy.field.delim) == 1) hierarchy.field.delim <- append(hierarchy.field.delim, hierarchy.field.delim)

      txt <- paste0(xml2::xml_find_all(node, paste0(xml2::xml_path(node),"/text()")), collapse = "")
      for(i in 1:length(ch)) {
        txt <- paste0(txt, hierarchy.field.delim[1], xml2::xml_name(ch[i]), hierarchy.value.sep, drilldown(ch[i], hierarchy.field.delim, hierarchy.value.sep, no.hierarchy), hierarchy.field.delim[2])
      }
      return(txt)
    }
  }
}


get_data <- function(rec, fields, only.fields, exclude.fields, field.names, hierarchy.field.delim, hierarchy.value.sep, no.hierarchy) {
  if(fields == "tags") {
    chd <- xml2::xml_children(rec)

    d<-c()
    for(i in 1:length(chd)) {
      d <- append(d, drilldown(chd[i], hierarchy.field.delim, hierarchy.value.sep, no.hierarchy))
    }

    if(is.null(field.names)) names(d) <- xml2::xml_name(chd)
    else names(d) <- xml2::xml_attr(chd, field.names)
  }
  else d <- xml2::xml_attrs(rec)
  d <- d[!names(d) %in% exclude.fields]
  if(!is.null(only.fields)) d <- d[names(d) %in% only.fields]

  return(d)
}



#' @title Converting XML to data frames and vice versa
#'
#' @description \code{xml_to_df()} converts XML data to a dataframe. It
#'   provides a lot of flexibility with its arguments but can usually be
#'   used with just a couple of them to achieve the desired results. See the
#'   examples below for simple applications.

#'@param file XML file to be converted. Instead of specifying a file, the XML
#'  code can put in directly via the \code{text} argument,
#'@param text XML code to be converted. Instead of providing the XML code, an
#'  XML file can be specified with the \code{file} argument.
#'@param first.records Number of records to be converted. If \code{NULL}
#'  (default) all records will be converted.
#'@param xml.encoding Encoding of the XML file (optional), e.g. (\code{"UTF-8"})
#'@param records.tags Name (or vector of names) of the tags that represent the
#'  data records in the XML (i.e. each record has one element with this tag
#'  name). All elements with this tag name will be considered data records.
#'  Instead of specifying the tag name, an XPatch expression can be used to
#'  identify the data records (see \code{records.xpath})
#'@param records.xpath XPath expression that specifies the XML element to be
#'  selected as data records; can be used instead of specifying the data record
#'  XML tags directly with the \code{data.records} argument. If both,
#'  \code{records.tags} and \code{records.path} are provided, only the XPath
#'  expressions determines the tags to be selected.
#'@param fields A character value, either \code{"tags"} or \code{"attributes"}.
#'  Specifies whether the fields of each data record are represented as XML tags
#'  or as attributes. See \emph{Details} below for more on this topic. Default
#'  is \code{"tags"}
#'@param field.names If the data fields are represented by XML elements/tags
#'  (i.e. \code{fields = "tags"}) and it is not the tag name that identifies the
#'  name of the data field but an attribute of that field tag then the name of
#'  the attribute that provides the field name can be specified here. If
#'  \code{NULL}, the tag names will be used as field names. See \emph{Details}
#'  for more information.
#'@param only.fields Optional vector of tag or attribute names (depending on the
#'  \code{fields} argument) of an XML record that will be included in the
#'  resulting dataframe. \code{NULL} means all fields found in the data will end
#'  up as columns in the dataframe.
#'@param exclude.fields Optional vector of fields that will be excluded from the
#'  conversion; fields specified here will not end up as columns in the
#'  resulting dataframe
#'@param check.datatypes Logical option that specifies if \code{xml_to_df()}
#'  tries to identify the data types of the fields in the XML data. If
#'  \code{TRUE} (default), \code{xml_to_df()} tries to identify numeric fields
#'  and changes the class of the respective columns in the resulting dataframe
#'  accordingly. Use the \code{data.dec} and \code{data.thds} arguments to
#'  specify a number format different from the standard US/EN scheme. At this
#'  point, there is no data type identification for logical and time/date values
#'  available. If \code{check.datatypes} equals \code{FALSE} then all variables
#'  in the resulting dataframe will be of class \code{character}
#'@param data.dec A decimal separator used in the identification of numeric data
#'  when \code{check.datatypes = TRUE}. Default is dot (\code{.})
#'@param data.thds A thousands separator used in the identification of numeric
#'  data when \code{check.datatypes = TRUE}. Default is comma (\code{,})
#'@param stringsAsFactors Logical option specifying if character values will be
#'  converted to factors in the resulting data frame. Is only applied if
#'  \code{check.datatypes = TRUE} which is the default
#'@param na Value that will be put into the resulting dataframe if the XML data
#'  field is \emph{empty}. Default is \code{NA}. If a data record in the XML
#'  does not have a specific field at all it is filled with the value provided
#'  via the \code{non.exist} argument
#'@param non.exist Value that will be put into the resulting dataframe if a data
#'  record in the XML data does not have a specific field at all. Default is the
#'  value of the \code{na} (the default of which is \code{NA}). If instead a
#'  field is present in the XML data but empty, then it will have the value of
#'  the \code{na} argument in the resulting data frame
#'@param no.hierarchy If the fields in the XML data are represented by XML
#'  elements/tags (i.e. argument \code{fields = "tags"}) and there is a
#'  hierarchical structure below a data field element then this hierarchical
#'  structure gets 'flattened', i.e. it will be represented by a single
#'  character value. See \emph{Details} for an example
#'@param hierarchy.field.delim One or two-element character vector specifying
#'  the tag delimiters used when 'flattening' a hierarchy in the XML data. If
#'  only one delimiter is specified then this delimiter will be used for both,
#'  the beginning of the tag and the end of the tag. See \emph{Details} for an
#'  example
#'@param hierarchy.value.sep Character value that is used as the separator
#'  between the tag name and the value of the tag when 'flattening' a hierarchy
#'  in the XML data. See \emph{Details} for an example
#'@param no.return Logical option to prevent \code{xml_to_df()} from returning
#'  the dataframe it creates; use this if you are only interested in saving the
#'  dataframe as Excel or CSV.
#'@param excel.filename Name of an Excel file the resulting dataframe will be
#'  exported to. If \code{NULL} (default) there will be no Excel export.
#'@param excel.sheetname  Name of the worksheet the resulting dataframe will be
#'  exported to when using the Excel export via the \code{excel.filename}
#'  argument. If \code{NULL}, \code{xml_to_df()} will figure out a name,
#'@param excel.pw Password that is applied to the Excel workbook when the
#'  resulting data.frame is exported via the \code{excel.filename} argument.
#'  Default {NULL} means the workbook will not be protected with a password
#'@param csv.filename Name of a CSV file the resulting dataframe will be
#'  exported to. If \code{NULL} there will be no CSV export.
#'@param csv.sep Separator used to separate fields in the CSV file when
#'  exporting the resulting dataframe via the \code{csv.filename} argument.
#'  Default is comma (\code{","})
#'@param csv.dec Decimal separator used when exporting the resulting dataframe
#'  via the \code{csv.filename} argument, Default is dot (\code{.})
#'@param csv.encoding Text encoding used when exporting the resulting dataframe
#'  via the \code{csv.filename} argument
#'@param ... Additional arguments passed on the \code{write.table()} when
#'  exporting the resulting dataframe via the \code{csv.filename} argument,
#'  Default is dot (\code{.})
#'
#'@return The resulting dataframe. There is no return value if the
#'  \code{no.return} argument is set to \code{TRUE}.
#'
#'@details This section provides some more details on how \code{xml_to_df()}
#'  works with different ways of representing data fields in the XML (tags
#'  versus attributes) and on working with nested XML field structures.\cr\cr
#'  \subsection{Two ways of representing the data: Tags and attributes}{ For
#'  \code{xml_to_df()} records are always represented by tags (i.e XML
#'  elements). Data fields within a record, however, can be represented by
#'  either tags or attributes.\cr\cr In the former case the XML would like like
#'  this:\cr \code{ <xml>} \cr \code{....<record>} \cr
#'  \code{........<field1>Value 1-1</field1>} \cr \code{........<field2>Value
#'  1-2</field2>} \cr \code{....</record>} \cr \code{....<record>} \cr
#'  \code{........<field1>Value 2-1</field1>} \cr \code{........<field2>Value
#'  2-2</field2>} \cr \code{....</record>} \cr \code{....</xml>} \cr\cr Here,
#'  each data field is represented by its own tag (e.g. \code{field1}). In this
#'  case the \code{records.tag} argument would need to be \code{"record"} (or we
#'  would specify an XPath expression with \code{records.xpath} that selects
#'  these elements) as this is the name of the tags that carry the data records;
#'  the \code{fields} argument would need to be \code{"tags"} because the actual
#'  data fields are represented by tags nested into the record elements.\cr A
#'  variant of this case would be if the fields are represented by tags but the
#'  field names are not the tag names but are provided by some attribute of
#'  these tags. This is the case in the following example:\cr \code{ <xml>} \cr
#'  \code{....<record>} \cr \code{........<data name="field1">Value 1-1</data>}
#'  \cr \code{........<data name="field2">Value 1-2</data>} \cr
#'  \code{....</record>} \cr \code{....<record>} \cr \code{........<data
#'  name="field1">Value 2-1</data>} \cr \code{........<data name"field2">Value
#'  2-2</data>} \cr \code{....</record>} \cr \code{....</xml>} \cr\cr  Here, we
#'  would use the optional \code{field.names} argument to tell
#'  \code{xml_to_df()} with \code{field.names = "name"} that each data tag has
#'  an attribute called \code{"name"} that specifies the field name for this
#'  data field tag.\cr\cr In contrast to these cases, data fields can also be
#'  represented by attributes of the record:\cr \code{<xml>}\cr
#'  \code{....<record field1="Value 1-1" field2="Value 1-2" />}\cr
#'  \code{....<record field1="Value 2-1" field2="Value 2-2" />}\cr
#'  \code{</xml>}\cr Here would need to change the \code{fields} argument to
#'  \code{"attributes"}.} \subsection{Working with the nested field values}{
#'  When data fields are represented by XML elements / tag then there may nested
#'  structures beneath them. If the \code{no.hierarchy} argument is \code{FALSE}
#'  (default) this nested structure within a field is recursively analyzed and
#'  represented in a single character value for this field. Each nested element
#'  is enclosed in the delimiters provided with the \code{hierarchy.field.delim}
#'  argument. By default, there is only one such delimiter (and that is
#'  \code{"|"}) which is used mark both, the start and the end of an element in
#'  the resulting value. However, it is possible to specify to different symbols
#'  in order to distinguish start and end of an element. The
#'  \code{hierarchy.value.sep} argument is the symbol that separates the name of
#'  the argument from its value. Consider the following example:\cr\cr
#'  \code{<xml>}\cr \code{....<ship>}\cr \code{........<name>Excelsior<name>}\cr
#'  \code{........<lastcaptain>Hikaru Sulu</lastcaptain>}\cr
#'  \code{....</ship>}\cr \code{....<ship>}\cr \code{........One proud ship
#'  name, many captains}\cr \code{........<name>Enterprise<name>}\cr
#'  \code{........<lastcaptain>}\cr \code{............<NCC-1701-A>James Tiberius
#'  Kirk</NCC-1701-A>}\cr \code{............<NCC-1701-B>John
#'  Harriman</NCC-1701-B>}\cr \code{............<NCC-1701-C>Rachel
#'  Garrett</NCC-1701-C>}\cr \code{............<NCC-1701-D>Jean-Luc
#'  Picard</NCC-1701-D>}\cr \code{........</lastcaptain>}\cr
#'  \code{....</ship>}\cr \code{</xml>}\cr\cr Calling \code{xml_to_df()} with
#'  the default values for \code{hierarchy.field.delim} and
#'  \code{hierarchy.value.sep} would result in the following value of the
#'  \code{lastcapatin} field for the \code{Enterprise} record:\cr \code{One
#'  proud name, many captains|NCC-1701-A~James Tiberius Kirk||NCC-1701-B~John
#'  Harriman||NCC-1701-C~Rachel Garrett||NCC-1701-D~Jean-Luc Picard|}\cr\cr If
#'  we would use \code{hierarchy.field.delim = c("[", "]")} then we would better
#'  see the start and of end of each element:\cr \code{One proud name, many
#'  captains[NCC-1701-A~James Tiberius Kirk][NCC-1701-B~John
#'  Harriman][NCC-1701-C~Rachel Garrett][NCC-1701-D~Jean-Luc Picard]} }
#'
#'
#' @examples
#' # Data used: World population figures from the United Nations Statistics Division
#'
#' # Read in the raw XML; two versions: one with data fields as XML
#' # elements/tags, one with data fields as attributes
#' example.tags <- system.file("worldpopulation_tags.xml", package="xmlconvert")
#' example.attr <- system.file("worldpopulation_attr.xml", package="xmlconvert")
#'
#' # Convert XML data to dataframe
#' worldpop.tags <- xml_to_df(text = example.tags, records.tags = c("record"),
#'     fields = "tags", field.names = "name")
#' worldpop.attr <- xml_to_df(text = example.attr, records.tags = c("record"),
#'     fields = "attributes")
#'
#'@family xmlconvert
#'
#'@export
xml_to_df <- function(file = NULL, text = NULL, first.records = NULL, xml.encoding = "", records.tags = NULL, records.xpath = NULL,
                      fields = "tags", field.names = NULL, only.fields = NULL, exclude.fields = NULL,
                      check.datatypes = TRUE, data.dec = ".", data.thds = ",", stringsAsFactors= FALSE,
                      na = NA, non.exist = na, no.hierarchy = FALSE, hierarchy.field.delim = "|", hierarchy.value.sep = "~",
                      no.return = FALSE, excel.filename = NULL, excel.sheetname = NULL, excel.pw = NULL,
                      csv.filename = NULL, csv.sep = ",", csv.dec = ".", csv.encoding = "", ...) {

  if(((ifnull(file, "") == "") + (ifnull(text, "") == "")) == 2) stop(
    "You need to provide either a file name (argument file) or the XML code itself (argument 'text').")

  if(((ifnull(records.tags, "") == "") + (ifnull(records.xpath, "") == "")) == 2) stop(
    "You need to provide either a tagQ name (argument 'records.tag') or an xpath expression (argument 'records.xpath') that describes the XML tags which carry the actual data records in your XML file.")

  if(ifnull(file, "") != "") {
    if(file.exists(file) | url_exists(file)) text <- readr::read_file(file)
    else stop(paste0("File or URL '", file, "' could not be opened."))
  }


  xml <- xml2::read_xml(text, encoding = xml.encoding)

  xp <- ""
  if(!is.null(records.tags)) xp <- paste0("//", records.tags, collapse = " | ")
  if(!is.null(records.xpath)) xp <- paste(xp, paste0(records.xpath, collapse = " | "), sep = " | ")
  recs <- xml2::xml_find_all(xml, xp)

  df <- data.frame()
  cols <- c()

  if(is.null(first.records)) first.records <- length(recs)
  for(i in 1:min(length(recs), first.records)) {
    dat <- get_data(recs[[i]], fields, only.fields, exclude.fields, field.names, hierarchy.field.delim, hierarchy.value.sep, no.hierarchy)
    dat[is.na(dat)] <- ""
    dat[dat == ""] <- na
    if(i != 1) {
      names.dat <- stringr::str_replace_all(names(dat), " ", "\\.")
      new <- names.dat[!names.dat %in% cols]
      if(length(new)) {
        cols <- append(cols, new)
        df[, new] <- non.exist
      }
      names(dat) <- names.dat
      rec.lst <- as.list(dat)
      rec.lst[[".data"]] <- df
      df <- do.call(tibble::add_row, rec.lst)
      df[i, !(names(df) %in% names(dat))] <- non.exist
    }
    else {
      df <- data.frame(as.list(dat), stringsAsFactors = FALSE)
      cols <- names(df)
    }
  }

  if(check.datatypes) {
    for(i in 1:NCOL(df)) {
      num.pattern <- paste0("^[0-9]*", escape(data.dec),"?[0-9]*$|^", escape(data.dec),"?[0-9]*$")
      num.check <- stringr::str_trim(stringr::str_replace(df[,i], data.thds, ""))
      if(sum(stringr::str_detect(num.check, num.pattern) & stringr::str_replace_na(num.check, "") != "", na.rm=TRUE) == NROW(num.check[stringr::str_replace_na(num.check, "") != ""])) {
        df[,i] <- stringr::str_replace(stringr::str_replace(df[,i], escape(data.dec), "."), escape(data.thds), "")
        df[,i] <- as.numeric(df[,i])
      }
      else {
        if(stringsAsFactors) df[,i] <- as.factor(df[,i])
      }
    }
  }

  # Outputs
  if(!is.null(excel.filename)) {
    if (requireNamespace("xlsx", quietly = TRUE)) {
      if(!is.null(excel.sheetname)) sh <- excel.sheetname
      else {
        if(!is.null(file)) {
          sh <- basename(file)
          if(stringr::str_detect(sh, "^.+\\.[:alnum:]{1,4}$")) sh <- stringr::str_match(sh, "^(.+)\\.[:alnum:]{1,4}$")[1,2]
        }
        else sh <- "xmlconvert results"
      }
      xlsx::write.xlsx2(df, excel.filename, sh, row.names = FALSE, show.NA=FALSE, password = excel.pw)
    }
    else {
      stop("Please install the 'xlsx' package with 'install.packages(\"xlsx\", dependencies = TRUE) if you want to use the Excel export functionality.", call. = FALSE)
    }
  }

  if(!is.null(csv.filename)) {
    utils::write.table(df, csv.filename, sep = csv.sep, dec = csv.dec, fileEncoding = csv.encoding, na = "", ...)
  }

  if(!no.return) return(df)
}



#' @title Converting XML to data frames and vice versa
#'
#' @description Converts dataframes to XML documents.
#'
#' @param df Dataframe to be converted to XML
#' @param fields A character value, either \code{"tags"} or \code{"attributes"}.
#'   Specifies whether the fields of each data record will be represented in the
#'   resulting XML document by XML tags or by attributes. See the \emph{Details}
#'   section of \code{\link{df_to_xml}()} for more on this topic. Default is
#'   \code{"tags"}
#' @param record.tag Name of the tags that will represent the data records in
#'   the resulting XML (i.e. each record has one element with this tag name).
#' @param field.names Names of the fields in the XML file. If \code{NULL} then
#'   the variable names from the dataframe are used. Field names are corrected
#'   automatically to comply with XML naming requirements.
#' @param only.fields Optional vector variable names from the dataframe that
#'   will be included in the XML document. If \code{NULL} then all fields from
#'   the dataframe are included in the XML document.
#' @param exclude.fields Optional vector of variable names from the dataframe
#'   that will not be included in the XML document.
#' @param root.node A character value with the desired name of the root element
#'   of the XML document; \code{"root"} by default.
#' @param xml.file Name of a file the XML document it written to. If \code{NULL}
#'   no file will be created.
#' @param non.exist Value that will be written into the XML document as field
#'   value in case the respective element in the dataframe is \code{NA}. If
#'   \code{NULL}, which is the default, no XML tag/attribute (depennding on the
#'   \code{fields} argument) will be created for this dataframe element. Using a
#'   non-\code{NULL} value for \code{non-exist} allows to make sure that each
#'   data record tag in the XML document has exactly the same structure even if
#'   some values may be empty (because they are \code{NA} in the original data)
#' @param encoding Encoding of the XML document; default is \code{"UTF-8"}
#' @param no.return  Logical option to prevent \code{df_to_xml()} from returning
#'   the XML document it creates; use this if you are only interested in saving
#'   the XML document to a file using the \code{xml.file} argument.
#'
#' @return The resulting XML document that be edited with the functions from the
#'   \pkg{xml2} package. There is no return value if the \code{no.return}
#'   argument is set to \code{TRUE}.
#'
#' @examples
#' # Create a dataframe
#' soccer.worldcups <- data.frame(list(year=c(2014, 2010, 2006),
#'     location=c("Brazil", "South Africa", "Germany"),
#'     goals_scored=c(171, 145, 147),
#'     average_goals=c(2.7, 2.3, 2.4),
#'     average_attendance=c(57918, 49669,52491)),
#'     stringsAsFactors = FALSE)
#'
#' # Convert to XML with the fields (i.e. dataframe variables/columns) stored in XML tags
#' xml <- df_to_xml(soccer.worldcups, fields="tags", record.tag = "worldcup")
#'
#' # Convert to XML with the fields (i.e. dataframe variables/columns) stored in attributes
#' xml <- df_to_xml(soccer.worldcups, fields="tags", record.tag = "worldcup")
#'
#' @family xmlconvert
#'
#' @export
df_to_xml <- function(df, fields = "tags", record.tag = "record", field.names = NULL, only.fields = NULL, exclude.fields = NULL,
                      root.node = "root", xml.file = NULL, non.exist = NULL, encoding = "UTF-8", no.return = FALSE) {

  xml <- xml2::xml_new_root(root.node, encoding = encoding)

  if(!is.null(only.fields)) df <- df[, only.fields]
  else {
    if(!is.null(exclude.fields)) df <- df[, names(df)[!names(df) %in% exclude.fields]]
  }

  if(is.null(field.names)) field.names <- names(df)
  else {
    if(length(field.names) != length(names(df))) {
      #if(length(names(df)) > 1)
      stop(paste0("Number of field names provided through the 'field.names' argument (", length(field.names), ") does not match the number of fields to be exported from the dataframe (", length(names(df)),"). Data fields to be exported are: ", paste0(paste0("'", names(df), "'"), collate=",") ))
      #else stop(paste0("Number of field names provided through the 'field.names' argument (", length(field.names), ") does not match the number of fields to be exported from the dataframe (", length(names(df)),"). Data fields to be exported are: ", names(df)))
    }
  }
  field.names <- stringr::str_replace_all(field.names, "[^\\-|[:alnum:]|_|\\.]", "_")
  field.names <- stringr::str_replace_all(field.names, "^([^[:lower:]|[:upper:]|_]+)", "_\\1")

  for(i in 1:NROW(df)) {
    rec <- xml2::xml_add_child(xml, record.tag)
    for(f in 1:NCOL(df)) {
      if(tolower(fields) == "tags") {
        if(!is.na(df[i,f]) | !is.null(non.exist)) {
          fld <- xml2::xml_add_child(rec, field.names[f])
          xml2::xml_set_text(fld, stringr::str_replace_na(as.character(df[i,f]), ""))
        }
      }
      else {
        vals <- as.character(df[i,])
        names(vals) <- field.names
        xml2::xml_set_attrs(rec, vals)
      }
    }
  }

  if(!is.null(xml.file)) xml2::write_xml(xml, xml.file, options = "format")
  if(!no.return) return(xml)
}



xml_is_numeric <- function(num) {
  return(suppressWarnings(!is.na(as.numeric(num))))
}


xml_convert_types <- function(elem, convert.types, dec, thsd, num.replace, datetime.formats) {
  if(convert.types & class(elem) != "list") {
    conv <- elem
    if(!is.null(datetime.formats)) {
      datetime.formats<-datetime.formats[order(nchar(datetime.formats))]
      conv.datetime <- NA
      for(i in 1:length(datetime.formats)) {
        test.datetime <- lubridate::as_date(conv, format=datetime.formats[i])
        if(!is.na(test.datetime)) conv.datetime <- test.datetime
      }
      if(!is.na(conv.datetime)) return(conv.datetime)
    }

    if(num.replace != "") conv <- stringr::str_replace_all(elem, escape(num.replace), "")
    if(thsd != "") conv <- stringr::str_replace_all(conv, escape(thsd), "")
    if(dec != "") conv <- stringr::str_replace_all(conv, escape(dec), ".")
    conv <- stringr::str_trim(conv, "both")
    if(xml_is_numeric(conv)) return(as.numeric(conv))
    else return(elem)
  }
  else return(elem)
}


process_xml_list <- function(elem, convert.types, dec, thsd, num.replace, datetime.formats) {
  len <- length(elem)
  if(class(elem) == "list") {
    if(len == 1) {
      if(class(elem[[1]]) == "list") elem <- process_xml_list(elem[[1]], convert.types, dec, thsd, num.replace, datetime.formats)
      else return(xml_convert_types(elem[[1]], convert.types, dec, thsd, num.replace, datetime.formats))
    }
    else {
      if(len > 0) {
        for(i in 1:len) {
          elem[[i]] <- process_xml_list(elem[[i]], convert.types, dec, thsd, num.replace, datetime.formats)
        }
      }
      else return(xml_convert_types(elem, convert.types, dec, thsd, num.replace, datetime.formats))
    }
  }
  return(xml_convert_types(elem, convert.types, dec, thsd, num.replace, datetime.formats))
}


remove_empty_elements <- function(lst) {
  for(i in length(lst):1) {
    if(length(lst[[i]]) == 0) lst[[i]] <- NULL
    else {
      if(class(lst[[i]]) == "list") lst[[i]] <- remove_empty_elements(lst[[i]])
    }
  }
  return(lst)
}



#' Converting XML documents to lists
#'
#' @description Converts XML documents to lists. Uses the
#'   \code{\link[xml2:as_list]{as_list()}} function from the
#'   \code{xml2} package but improves its output. As an effect, numbers, dates
#'   and times are converted correctly, unnecessary nested sub-lists with only
#'   element are avoided, and empty XML nodes can be removed altogether. This
#'   makes the resulting list look cleaner and better structured.
#'
#' @param xml XML document to be converted. Can be read in from a file using
#'   \code{xml2}'s \code{read_xml()} function.
#' @param cleanup If \code{TRUE} (default) empty XML nodes (with no sub-nodes or
#'   values) will not appear in the list.
#' @param convert.types If \code{TRUE} (default) \code{xml_to_list()} will try
#'   to infer the data type of value elements in the XML. If \code{FALSE}, all
#'   value elements in the resulting list will be of type \code{character}.
#' @param dec Decimal separator used in numbers.
#' @param thsd Thousands separator used in numbers.
#' @param num.replace An optional string that will be removed before
#'   \code{xml_to_list()} tries to convert values to numbers. Can be used, for
#'   example, to remove currency symbols or other measurement units from values
#'   that are actually numerical.
#' @param datetime.formats A vector of date and/or time formats that will be
#'   used to recognize the respective datatypes. Formats will need to be written
#'   in the general notation used by \code{\link[base:strftime]{strftime()}} and
#'   other standard R functions.
#'
#' @return A \code{list} object representing the XML document.

#' @examples
#' xml <- xml2::read_xml(system.file("customers.xml", package="xmlconvert"))
#' xml.list <- xml_to_list(xml, num.replace="USD", datetime.formats = "%Y-%m-%d")#'
#'
#'@export
xml_to_list <- function(xml, cleanup = TRUE, convert.types = TRUE, dec =".", thsd = ",", num.replace = "", datetime.formats = NULL) {
  xml <- xml2::as_list(xml)
  lst <- process_xml_list(xml, convert.types, dec, thsd, num.replace, datetime.formats)
  if(cleanup) lst <- remove_empty_elements(lst)
  return(lst)
}
