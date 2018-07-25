CLASS ltcl_check_branch_overview DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      check_compress FOR TESTING RAISING cx_static_check.
    METHODS when_compress RAISING zcx_abapgit_exception.
    METHODS given_branch_overview RAISING zcx_abapgit_exception.
    METHODS given_commits.
    METHODS given_commits_expected.
    METHODS then_compressed_like_expected.

    DATA:
      mo_repo_online      TYPE REF TO zcl_abapgit_repo_online,
      mi_branch_overview  TYPE REF TO zif_abapgit_branch_overview,
      mt_commits          TYPE zif_abapgit_definitions=>ty_commit_tt,
      mt_commits_expected TYPE zif_abapgit_definitions=>ty_commit_tt..

ENDCLASS.



CLASS ltcl_check_branch_overview IMPLEMENTATION.

  METHOD check_compress.

    given_branch_overview( ).
    given_commits( ).
    given_commits_expected( ).
    when_compress( ).
    then_compressed_like_expected( ).

  ENDMETHOD.


  METHOD when_compress.

    mt_commits = mi_branch_overview->compress( it_commits = mt_commits ).

  ENDMETHOD.


  METHOD given_branch_overview.

    DATA: ls_data TYPE zif_abapgit_persistence=>ty_repo.

    ls_data-key     = 'dummmy'.
    ls_data-package = '$DUMMY'.

    CREATE OBJECT mo_repo_online
      EXPORTING
        is_data = ls_data.

    "TODO: Mock for online repo needed before branch overview can be testet
    mi_branch_overview = zcl_abapgit_factory=>get_branch_overview( io_repo = mo_repo_online ).


  ENDMETHOD.


  METHOD given_commits.

  ENDMETHOD.


  METHOD given_commits_expected.

  ENDMETHOD.


  METHOD then_compressed_like_expected.

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = mt_commits    " Data object with current value
        exp                  = mt_commits_expected    " Data object with expected type
    ).

  ENDMETHOD.

ENDCLASS.
