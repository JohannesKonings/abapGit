CLASS zcl_abapgit_object_sprx DEFINITION
INHERITING FROM zcl_abapgit_objects_super
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      zif_abapgit_object.
    METHODS constructor
      IMPORTING
                is_item     TYPE zif_abapgit_definitions=>ty_item
                iv_language TYPE spras
      RAISING   zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_proxy_data      TYPE string VALUE 'PROXY_DATA' ##NO_TEXT.
    CONSTANTS c_proxy_header    TYPE string VALUE 'PROXY_HEADER' ##NO_TEXT.

    METHODS serialize_xml
      IMPORTING
        iv_object   TYPE sproxhdr-object
        iv_obj_name TYPE sproxhdr-obj_name
        io_xml      TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception.
    METHODS get_object_and_name
      EXPORTING
        ev_object   TYPE sproxhdr-object
        ev_obj_name TYPE sproxhdr-obj_name.
    METHODS corr_insert_sprx
      IMPORTING
        iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception.
    METHODS is_supported
      RETURNING
                VALUE(rv_is_supported) TYPE abap_bool
      RAISING   zcx_abapgit_exception.

    DATA mv_object   TYPE sproxhdr-object.
    DATA mv_obj_name TYPE sproxhdr-obj_name.

ENDCLASS.



CLASS zcl_abapgit_object_sprx IMPLEMENTATION.


  METHOD constructor.

    super->constructor( is_item = is_item iv_language = iv_language ).

    get_object_and_name(
      IMPORTING
        ev_object   = mv_object
        ev_obj_name = mv_obj_name ).

    IF is_supported( ) = abap_false.
      zcx_abapgit_exception=>raise( |SPRX - not supported: { is_item-obj_name }| ).
    ENDIF.

  ENDMETHOD.


  METHOD corr_insert_sprx.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = mv_obj_name
        object_class        = mv_object
        devclass            = iv_package
        master_language     = mv_language
        mode                = 'INSERT'
        global_lock         = abap_true
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc = 1.
      zcx_abapgit_exception=>raise( 'Cancelled' ).
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from RS_CORR_INSERT' ).
    ENDIF.

  ENDMETHOD.


  METHOD get_object_and_name.

    ev_object   = ms_item-obj_name(4).
    ev_obj_name = ms_item-obj_name+4.

  ENDMETHOD.


  METHOD serialize_xml.

    DATA lo_proxy           TYPE REF TO cl_proxy.
    DATA ls_sprx_db_data    TYPE sprx_db_data.
    DATA lt_delta           TYPE sprx_t_delta.

    FIELD-SYMBOLS <ls_sproxheader>     LIKE LINE OF ls_sprx_db_data-sproxhdr.

    TRY.
        lo_proxy = cl_proxy_fact=>load_by_abap_name(
            object             = mv_object
            obj_name           = mv_obj_name ).


        lt_delta = lo_proxy->get_delta_all( ).

        ls_sprx_db_data = cl_proxy_db=>serialize(
                                          proxy     = lo_proxy
                                          inactive  = abap_false
                                          delta     = lt_delta ).

        LOOP AT ls_sprx_db_data-sproxhdr ASSIGNING <ls_sproxheader>.

          CLEAR <ls_sproxheader>-created_by.
          CLEAR <ls_sproxheader>-created_on.
          CLEAR <ls_sproxheader>-changed_by.
          CLEAR <ls_sproxheader>-changed_on.

        ENDLOOP.

        io_xml->add(
               iv_name               = c_proxy_header
               ig_data               = ls_sprx_db_data-sproxhdr ).

        io_xml->add(
            iv_name               = c_proxy_data
            ig_data               = ls_sprx_db_data-sproxdat ).

      CATCH cx_proxy_gen_error.
        zcx_abapgit_exception=>raise( |SPRX - error load proxy { iv_obj_name }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA lv_changed_by TYPE sproxhdr-changed_by.

    SELECT SINGLE changed_by
    FROM sproxhdr
    INTO lv_changed_by
    WHERE object     = mv_object
    AND   obj_name   = mv_obj_name
    AND   inactive   = abap_false.

    IF sy-subrc = 0.
      IF lv_changed_by IS NOT INITIAL.
        rv_user = lv_changed_by.
      ELSE.
        rv_user = c_user_unknown.
      ENDIF.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA lv_object      TYPE sproxhdr-object.
    DATA lv_obj_name    TYPE sproxhdr-obj_name.

    DATA lv_return_code TYPE i.
    DATA lt_log         TYPE sprx_log_t.

    get_object_and_name(
      IMPORTING
        ev_object   = lv_object
        ev_obj_name = lv_obj_name ).

    cl_proxy_data=>delete_single_proxy(
      EXPORTING
        object           = lv_object
        obj_name         = lv_obj_name
    CHANGING
      c_return_code    = lv_return_code
      ct_log           = lt_log ).
    IF lv_return_code <> 0.
      zcx_abapgit_exception=>raise( 'SPRX: Error from DELETE_SINGLE_PROXY' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA lt_sproxhdr_old    TYPE sprx_hdr_t.
    DATA lt_sproxdat_old    TYPE sprx_dat_t.
    DATA lt_sproxsvar_old   TYPE sprx_svar_t.
    DATA lt_sproxintf_old   TYPE sprx_matchintf_t.
    DATA lt_sproxhdr_new    TYPE sprx_hdr_t.
    DATA lt_sproxdat_new    TYPE sprx_dat_t.
    DATA lt_sproxsvar_new   TYPE sprx_svar_t.
    DATA lt_sproxintf_new   TYPE sprx_matchintf_t.

    DATA lo_proxy           TYPE REF TO cl_proxy.
    DATA lt_delta           TYPE sprx_t_delta.
    DATA ls_db_data         TYPE sprx_db_data.

    DATA lt_abap_keys       TYPE prx_abapobjects.
    DATA ls_abap_key        LIKE LINE OF lt_abap_keys.
    DATA lt_log             TYPE sprx_log_t.

    corr_insert_sprx( iv_package ).

    "add Delta-Handling to avoid that single objects created without the dependent objects.
    "Thereby the dependent objects will delete
    TRY.
        lo_proxy = cl_proxy_fact=>load_by_abap_name(
                   object             = mv_object
                   obj_name           = mv_obj_name ).


        lt_delta = lo_proxy->get_delta_all( ).

        ls_db_data = cl_proxy_db=>serialize(
                                          proxy     = lo_proxy
                                          inactive  = abap_false
                                          delta     = lt_delta ).

        lt_sproxhdr_new = ls_db_data-sproxhdr.
        lt_sproxdat_new = ls_db_data-sproxdat.


      CATCH cx_proxy_gen_error.
        "No delta for this object -> create

        io_xml->read(
          EXPORTING
            iv_name               = c_proxy_header
          CHANGING
            cg_data               = lt_sproxhdr_new ).

        IF lt_sproxhdr_new IS NOT INITIAL.

          io_xml->read(
            EXPORTING
              iv_name               = c_proxy_data
            CHANGING
              cg_data               = lt_sproxdat_new ).


        ELSE.
          zcx_abapgit_exception=>raise( |SPRX - error deserialize: { ms_item-obj_name }| ).
        ENDIF.


    ENDTRY.

    cl_proxy_data=>db_save(
        sproxhdr_old  = lt_sproxhdr_old
        sproxdat_old  = lt_sproxdat_old
        sproxsvar_old = lt_sproxsvar_old
        sproxintf_old = lt_sproxintf_old
        sproxhdr_new  = lt_sproxhdr_new
        sproxdat_new  = lt_sproxdat_new
        sproxsvar_new = lt_sproxsvar_new
        sproxintf_new = lt_sproxintf_new ).

    ls_abap_key-object = mv_object.
    ls_abap_key-obj_name = mv_obj_name.

    APPEND ls_abap_key TO lt_abap_keys.

    TRY.
        cl_proxy_utils=>check_sprx_tadir(   EXPORTING   objects = lt_abap_keys
                                                        repair  = abap_true
                                            IMPORTING log = lt_log ).
      CATCH cx_proxy_gen_error.
        zcx_abapgit_exception=>raise( |SPRX - error deserialize: { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA lv_status  TYPE prx_status.
    DATA lv_status_text  TYPE prx_status_t.


    cl_proxy_data=>db_get_status(
      EXPORTING
        object      = mv_object
        obj_name    = mv_obj_name
      IMPORTING
        status      = lv_status
        status_text = lv_status_text ).
    IF lv_status = if_proxy=>c_state_active.
      rv_bool = abap_true.
    ELSE.
      rv_bool = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.

    rs_metadata-class = cl_abap_classdescr=>describe_by_object_ref( me )->get_relative_name( ).
    rs_metadata-version = 'v1.0.0' ##no_text.

  ENDMETHOD.


  METHOD zif_abapgit_object~has_changed_since.

  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'SPRX'
        in_new_window = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA ls_item            LIKE ms_item.
    DATA ls_files_and_item  TYPE zcl_abapgit_objects=>ty_serialization.
    DATA ls_files           LIKE LINE OF ls_files_and_item-files.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.


    ls_item-obj_type    = mv_object.
    ls_item-obj_name    = mv_obj_name.
    ls_item-devclass    = ms_item-devclass.

    ls_files_and_item = zcl_abapgit_objects=>serialize(
                                  is_item               = ls_item
                                  iv_language           = mv_language ).

    CASE mv_object.

      WHEN 'CLAS'.

        LOOP AT ls_files_and_item-files INTO ls_files WHERE filename CP '*.abap'.

          me->zif_abapgit_object~mo_files->add_raw(
              iv_ext                = 'abap'
              iv_data               = ls_files-data ).

        ENDLOOP.

    ENDCASE.

    serialize_xml(
        iv_object   = mv_object
        iv_obj_name = mv_obj_name
        io_xml      = io_xml ).

  ENDMETHOD.

  METHOD is_supported.

    DATA lo_proxy           TYPE REF TO cl_proxy.
    DATA ls_sprx_db_data    TYPE sprx_db_data.
    DATA lt_delta           TYPE sprx_t_delta.

    FIELD-SYMBOLS <ls_sproxheader>     LIKE LINE OF ls_sprx_db_data-sproxhdr.

    rv_is_supported = abap_true.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    TRY.
        lo_proxy = cl_proxy_fact=>load_by_abap_name(
            object             = mv_object
            obj_name           = mv_obj_name ).


        lt_delta = lo_proxy->get_delta_all( ).

        ls_sprx_db_data = cl_proxy_db=>serialize(
                                          proxy     = lo_proxy
                                          inactive  = abap_false
                                          delta     = lt_delta ).

        "Only Service Consumer allowed. Service Provider will come later
        LOOP AT ls_sprx_db_data-sproxhdr ASSIGNING <ls_sproxheader>.
          IF <ls_sproxheader>-direction = 'I'.
            rv_is_supported = abap_false.
            EXIT.
          ENDIF.
        ENDLOOP.

      CATCH cx_proxy_gen_error.
        zcx_abapgit_exception=>raise( |SPRX - error load proxy { mv_obj_name }| ).
    ENDTRY.

  ENDMETHOD.

  METHOD zif_abapgit_object~is_active.
    rv_active = abap_true. "dummy implementation
  ENDMETHOD.

ENDCLASS.
