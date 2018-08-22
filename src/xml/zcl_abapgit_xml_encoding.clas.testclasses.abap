CLASS ltcl_xml_bom DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS delete_xml_bom_utf8 FOR TESTING RAISING zcx_abapgit_exception.
    METHODS get_xml_xstring_with_bom RETURNING VALUE(rv_xml_xstring_with_bom) TYPE xstring.
    METHODS get_xml_xstring_without_bom RETURNING VALUE(rv_xml_xstring_without_bom) TYPE xstring.

ENDCLASS.

CLASS ltcl_xml_bom IMPLEMENTATION.




  METHOD delete_xml_bom_utf8.

    DATA: lv_xml             TYPE string,
          lv_xml_xstring_act TYPE xstring,
          lv_xml_xstring_exp TYPE xstring.

    "With BOM

    lv_xml_xstring_act = get_xml_xstring_with_bom( ).

    lv_xml = zcl_abapgit_convert=>xstring_to_string_utf8( lv_xml_xstring_act ).

    lv_xml = zcl_abapgit_xml_encoding=>change_encoding_to_utf8( iv_xml = lv_xml ).

    lv_xml_xstring_act = zcl_abapgit_convert=>string_to_xstring_utf8( lv_xml ).

    lv_xml_xstring_exp = get_xml_xstring_without_bom( ).

    cl_abap_unit_assert=>assert_equals( act = lv_xml_xstring_act
                                        exp = lv_xml_xstring_exp ).

    "Without BOM

    lv_xml_xstring_act = get_xml_xstring_without_bom( ).

    lv_xml = zcl_abapgit_convert=>xstring_to_string_utf8( lv_xml_xstring_act ).

    lv_xml = zcl_abapgit_xml_encoding=>change_encoding_to_utf8( iv_xml = lv_xml ).

    lv_xml_xstring_act = zcl_abapgit_convert=>string_to_xstring_utf8( lv_xml ).

    lv_xml_xstring_exp = get_xml_xstring_without_bom( ).

    cl_abap_unit_assert=>assert_equals( act = lv_xml_xstring_act
                                        exp = lv_xml_xstring_exp ).


  ENDMETHOD.


  METHOD get_xml_xstring_without_bom.

    rv_xml_xstring_without_bom = '3C3F786D6C2076657273696F6E' &&
                                 '3D22312E302220656E636F64696E673D' &&
                                 '227574662D38223F3E0D0A3C6173783A' &&
                                 '6162617020786D6C6E733A6173783D22' &&
                                 '687474703A2F2F7777772E7361702E63' &&
                                 '6F6D2F61626170786D6C222076657273' &&
                                 '696F6E3D22312E30223E0D0A203C6173' &&
                                 '783A76616C7565733E0D0A20203C4441' &&
                                 '54413E0D0A2020203C4D41535445525F' &&
                                 '4C414E47554147453E453C2F4D415354' &&
                                 '45525F4C414E47554147453E0D0A2020' &&
                                 '203C5354415254494E475F464F4C4445' &&
                                 '523E2F7372632F3C2F5354415254494E' &&
                                 '475F464F4C4445523E0D0A2020203C46' &&
                                 '4F4C4445525F4C4F4749433E50524546' &&
                                 '49583C2F464F4C4445525F4C4F474943' &&
                                 '3E0D0A2020203C49474E4F52453E0D0A' &&
                                 '202020203C6974656D3E2F2E74726176' &&
                                 '69732E796D6C3C2F6974656D3E0D0A20' &&
                                 '2020203C6974656D3E2F434F4E545249' &&
                                 '425554494E472E6D643C2F6974656D3E' &&
                                 '0D0A202020203C6974656D3E2F4C4943' &&
                                 '454E53453C2F6974656D3E0D0A202020' &&
                                 '203C6974656D3E2F524541444D452E6D' &&
                                 '643C2F6974656D3E0D0A202020203C69' &&
                                 '74656D3E2F7061636B6167652E6A736F' &&
                                 '6E3C2F6974656D3E0D0A202020203C69' &&
                                 '74656D3E2F6368616E67656C6F672E74' &&
                                 '78743C2F6974656D3E0D0A202020203C' &&
                                 '6974656D3E2F2E67697469676E6F7265' &&
                                 '3C2F6974656D3E0D0A202020203C6974' &&
                                 '656D3E2F434F44455F4F465F434F4E44' &&
                                 '5543542E6D643C2F6974656D3E0D0A20' &&
                                 '20203C2F49474E4F52453E0D0A20203C' &&
                                 '2F444154413E0D0A203C2F6173783A76' &&
                                 '616C7565733E0D0A3C2F6173783A6162' &&
                                 '61703E0D0A'.


  ENDMETHOD.

  METHOD get_xml_xstring_with_bom.

    rv_xml_xstring_with_bom = 'EFBBBF3C3F786D6C2076657273696F6E' &&
                              '3D22312E302220656E636F64696E673D' &&
                              '227574662D38223F3E0D0A3C6173783A' &&
                              '6162617020786D6C6E733A6173783D22' &&
                              '687474703A2F2F7777772E7361702E63' &&
                              '6F6D2F61626170786D6C222076657273' &&
                              '696F6E3D22312E30223E0D0A203C6173' &&
                              '783A76616C7565733E0D0A20203C4441' &&
                              '54413E0D0A2020203C4D41535445525F' &&
                              '4C414E47554147453E453C2F4D415354' &&
                              '45525F4C414E47554147453E0D0A2020' &&
                              '203C5354415254494E475F464F4C4445' &&
                              '523E2F7372632F3C2F5354415254494E' &&
                              '475F464F4C4445523E0D0A2020203C46' &&
                              '4F4C4445525F4C4F4749433E50524546' &&
                              '49583C2F464F4C4445525F4C4F474943' &&
                              '3E0D0A2020203C49474E4F52453E0D0A' &&
                              '202020203C6974656D3E2F2E74726176' &&
                              '69732E796D6C3C2F6974656D3E0D0A20' &&
                              '2020203C6974656D3E2F434F4E545249' &&
                              '425554494E472E6D643C2F6974656D3E' &&
                              '0D0A202020203C6974656D3E2F4C4943' &&
                              '454E53453C2F6974656D3E0D0A202020' &&
                              '203C6974656D3E2F524541444D452E6D' &&
                              '643C2F6974656D3E0D0A202020203C69' &&
                              '74656D3E2F7061636B6167652E6A736F' &&
                              '6E3C2F6974656D3E0D0A202020203C69' &&
                              '74656D3E2F6368616E67656C6F672E74' &&
                              '78743C2F6974656D3E0D0A202020203C' &&
                              '6974656D3E2F2E67697469676E6F7265' &&
                              '3C2F6974656D3E0D0A202020203C6974' &&
                              '656D3E2F434F44455F4F465F434F4E44' &&
                              '5543542E6D643C2F6974656D3E0D0A20' &&
                              '20203C2F49474E4F52453E0D0A20203C' &&
                              '2F444154413E0D0A203C2F6173783A76' &&
                              '616C7565733E0D0A3C2F6173783A6162' &&
                              '61703E0D0A'.

  ENDMETHOD.

ENDCLASS.
