class ZCL_ABAPGIT_FACTORY definition
  public
  create private

  global friends ZCL_ABAPGIT_INJECTOR .

public section.

  class-methods GET_TADIR
    returning
      value(RI_TADIR) type ref to ZIF_ABAPGIT_TADIR .
  class-methods GET_SAP_PACKAGE
    importing
      !IV_PACKAGE type DEVCLASS
    returning
      value(RI_SAP_PACKAGE) type ref to ZIF_ABAPGIT_SAP_PACKAGE .
  class-methods GET_CODE_INSPECTOR
    importing
      !IV_PACKAGE type DEVCLASS
      !IV_CHECK_VARIANT_NAME type SCI_CHKV
    returning
      value(RI_CODE_INSPECTOR) type ref to ZIF_ABAPGIT_CODE_INSPECTOR
    raising
      ZCX_ABAPGIT_EXCEPTION .
  class-methods GET_SYNTAX_CHECK
    importing
      !IV_PACKAGE type DEVCLASS
    returning
      value(RI_SYNTAX_CHECK) type ref to ZIF_ABAPGIT_CODE_INSPECTOR
    raising
      ZCX_ABAPGIT_EXCEPTION .
  class-methods GET_BRANCH_OVERVIEW
    importing
      !IO_REPO type ref to ZCL_ABAPGIT_REPO_ONLINE
    returning
      value(RI_BRANCH_OVERVIEW) type ref to ZIF_ABAPGIT_BRANCH_OVERVIEW
    raising
      ZCX_ABAPGIT_EXCEPTION .
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_sap_package,
        package  TYPE devclass,
        instance TYPE REF TO zif_abapgit_sap_package,
      END OF ty_sap_package,
      tty_sap_package TYPE HASHED TABLE OF ty_sap_package
                      WITH UNIQUE KEY package,

      BEGIN OF ty_code_inspector,
        package            TYPE devclass,
        check_variant_name TYPE sci_chkv,
        instance           TYPE REF TO zif_abapgit_code_inspector,
      END OF ty_code_inspector,
      tty_code_inspector TYPE HASHED TABLE OF ty_code_inspector
                         WITH UNIQUE KEY package check_variant_name,
      BEGIN OF ty_syntax_check,
        package  TYPE devclass,
        instance TYPE REF TO zif_abapgit_code_inspector,
      END OF ty_syntax_check,
      tty_syntax_check TYPE HASHED TABLE OF ty_syntax_check
                       WITH UNIQUE KEY package,

      BEGIN OF ty_branch_overview,
        repo_key TYPE zif_abapgit_persistence=>ty_value,
        instance TYPE REF TO zif_abapgit_branch_overview,
      END OF ty_branch_overview,
      tty_branch_overview TYPE HASHED TABLE OF ty_branch_overview
                         WITH UNIQUE KEY repo_key.

    CLASS-DATA:
      gi_tadir           TYPE REF TO zif_abapgit_tadir,
      gt_sap_package     TYPE tty_sap_package,
      gt_code_inspector  TYPE tty_code_inspector,
      gt_syntax_check    TYPE tty_syntax_check,
      gt_branch_overview TYPE tty_branch_overview.

ENDCLASS.



CLASS ZCL_ABAPGIT_FACTORY IMPLEMENTATION.


  METHOD get_branch_overview.

    DATA: ls_branch_overview LIKE LINE OF gt_branch_overview,
          lv_repo_key        TYPE zif_abapgit_persistence=>ty_value.
    FIELD-SYMBOLS: <ls_branch_overview> TYPE zcl_abapgit_factory=>ty_branch_overview.

    lv_repo_key = io_repo->get_key( ).

    "The table is only used for ABAP Unit.
    "The application needs always a new instance because of reuse from the repo key from deleted repos for new repos
    READ TABLE gt_branch_overview ASSIGNING <ls_branch_overview>
                                  WITH TABLE KEY repo_key = lv_repo_key.
    IF sy-subrc <> 0.
      ls_branch_overview-repo_key =  lv_repo_key.

      CREATE OBJECT ls_branch_overview-instance
        TYPE zcl_abapgit_branch_overview
        EXPORTING
          io_repo = io_repo.

    ENDIF.

    ri_branch_overview = <ls_branch_overview>-instance.

  ENDMETHOD.


  METHOD get_code_inspector.

    DATA: ls_code_inspector LIKE LINE OF gt_code_inspector.
    FIELD-SYMBOLS: <ls_code_inspector> TYPE zcl_abapgit_factory=>ty_code_inspector.

    READ TABLE gt_code_inspector ASSIGNING <ls_code_inspector>
                                 WITH TABLE KEY package            = iv_package
                                                check_variant_name = iv_check_variant_name.
    IF sy-subrc <> 0.
      ls_code_inspector-package = iv_package.
      ls_code_inspector-check_variant_name = iv_check_variant_name.

      CREATE OBJECT ls_code_inspector-instance TYPE zcl_abapgit_code_inspector
        EXPORTING
          iv_package            = iv_package
          iv_check_variant_name = iv_check_variant_name.

      INSERT ls_code_inspector
             INTO TABLE gt_code_inspector
             ASSIGNING <ls_code_inspector>.

    ENDIF.

    ri_code_inspector = <ls_code_inspector>-instance.

  ENDMETHOD.


  METHOD get_sap_package.

    DATA: ls_sap_package TYPE ty_sap_package.
    FIELD-SYMBOLS: <ls_sap_package> TYPE ty_sap_package.

    READ TABLE gt_sap_package ASSIGNING <ls_sap_package>
                              WITH TABLE KEY package = iv_package.
    IF sy-subrc <> 0.

      ls_sap_package-package = iv_package.
      CREATE OBJECT ls_sap_package-instance TYPE zcl_abapgit_sap_package
        EXPORTING
          iv_package = iv_package.

      INSERT ls_sap_package
             INTO TABLE gt_sap_package
             ASSIGNING <ls_sap_package>.

    ENDIF.

    ri_sap_package = <ls_sap_package>-instance.

  ENDMETHOD.


  METHOD get_syntax_check.

    DATA: ls_syntax_check LIKE LINE OF gt_syntax_check.
    FIELD-SYMBOLS: <ls_syntax_check> TYPE zcl_abapgit_factory=>ty_syntax_check.

    READ TABLE gt_syntax_check ASSIGNING <ls_syntax_check>
                               WITH TABLE KEY package = iv_package.
    IF sy-subrc <> 0.
      ls_syntax_check-package =  iv_package.

      CREATE OBJECT ls_syntax_check-instance TYPE zcl_abapgit_syntax_check
        EXPORTING
          iv_package = iv_package.

      INSERT ls_syntax_check
             INTO TABLE gt_syntax_check
             ASSIGNING <ls_syntax_check>.

    ENDIF.

    ri_syntax_check = <ls_syntax_check>-instance.

  ENDMETHOD.


  METHOD get_tadir.

    IF gi_tadir IS INITIAL.
      CREATE OBJECT gi_tadir TYPE zcl_abapgit_tadir.
    ENDIF.

    ri_tadir = gi_tadir.

  ENDMETHOD.
ENDCLASS.
