# xmlconvert 0.1.2

## Minor changes

* New logical argument `strip.ns` in `xml_to_df()` to remove namespaces from the XML data (default is FALSE). If namespaces are present, `xml2::xml_find_all()` may fail to find the data-carrying XML tags in the XML data. Namespaces can now be removed by setting `strip.ns` to `TRUE`.


# xmlconvert 0.1.1

## Minor changes

* Added function `xml_to_list()` to properly convert XML data to R lists.
* Added optional argument `first.records` for function `xml_to_df()` to limit the number of XML data records to be converted into a dataframe (e.g. for testing purposes).


# xmlconvert 0.1.0

Initial version.
