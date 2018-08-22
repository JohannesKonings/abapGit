CLASS zcl_abapgit_xml_encoding DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS change_encoding_to_utf8 IMPORTING iv_xml        TYPE string
                                          RETURNING VALUE(rv_xml) TYPE string
                                          RAISING   zcx_abapgit_exception.


  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS delete_bom_utf8 IMPORTING iv_xml        TYPE string
                                  RETURNING VALUE(rv_xml) TYPE string
                                  RAISING   zcx_abapgit_exception.

    CLASS-METHODS is_utf8_bom IMPORTING iv_xml_xstring    TYPE xstring
                              RETURNING VALUE(rv_xml_bom) TYPE abap_bool.

ENDCLASS.



CLASS zcl_abapgit_xml_encoding IMPLEMENTATION.


  METHOD delete_bom_utf8.

    DATA lv_xml_xstring TYPE xstring.

    lv_xml_xstring = zcl_abapgit_convert=>string_to_xstring_utf8( iv_xml ).
    IF is_utf8_bom( lv_xml_xstring ) = abap_true.
      SHIFT lv_xml_xstring LEFT DELETING LEADING cl_abap_char_utilities=>byte_order_mark_utf8 IN BYTE MODE.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'error deleting BOM' ).
      ENDIF.
      rv_xml = zcl_abapgit_convert=>xstring_to_string_utf8( lv_xml_xstring ).
    ELSE.
      rv_xml = iv_xml.
    ENDIF.



  ENDMETHOD.


  METHOD is_utf8_bom.

    IF iv_xml_xstring(3) = cl_abap_char_utilities=>byte_order_mark_utf8.
      rv_xml_bom = abap_true.
    ELSE.
      rv_xml_bom = abap_false.
    ENDIF.

  ENDMETHOD.



  METHOD change_encoding_to_utf8.

    rv_xml = iv_xml.

    REPLACE FIRST OCCURRENCE
    OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
    IN rv_xml
    WITH '<?xml version="1.0" encoding="utf-8"?>'.
    IF sy-subrc = 0.
      rv_xml = delete_bom_utf8( iv_xml = rv_xml ).
    ELSE.
      zcx_abapgit_exception=>raise( 'error change to utf-8' ).
    ENDIF.



  ENDMETHOD.

ENDCLASS.
