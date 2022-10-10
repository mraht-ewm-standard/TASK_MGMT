"! Code Inspector Check: TODO Flags
"!
"! <p><strong>Installation Guide:</strong>
"! <ol>
"!  <li>Activate class: TCode 'SCI' → Menu → Code Inspector → Management of → Tests</li>
"!  <li>Select checkbox next to class (Entry most of the times at the end of the list) → Save</li>
"!  <li>TCode 'SCI': Edit global check variant profile 'DEFAULT'<br/>
"!      <strong>Note: </strong>If your not allowed to change the default variant, copy it to a new global Z-variant</li>
"!  <li>Search Functions → Activate 'Search for TODO Flags'</li>
"!  <li>Edit selection parameter:</li>
"!      <ol>
"!          <li>Activate comments and literals</li>
"!          <li>Choose message type = 'W'</li>
"!          <li>Search String → Multiple Selection → Add the following values:<br/>
"!          '##TODO', '##NEW', '##OPT', '##FIX'</li>
"!          <li>Confirm → Save</li>
"!      </ol>
"!  <li>TCode 'SE16N': Change table 'SCICHKV_ALTER' with entry 'DEFAULT' (field CHECKVNAME_DEF) to the new variant
"!  <br/><strong>Note: </strong>If you're not allowed to change the table each user has to set the check variant
"!       manually: In Eclipse right click on your project → Properties → ABAP Development → ABAP Test Cockpit<br/>
"!       Set 'Global Code Inspector check variant' to the new Z-variant → Apply and Close</li>
"! </ol>
"! </p>
CLASS ziot_cl_ci_check_todo_flags DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_test_free_search
  CREATE PUBLIC .

*"* public components of class CL_CI_TEST_SEARCH_ABAP_PATTERN
*"* do not include other source files here!!!
  PUBLIC SECTION.

    METHODS constructor .

    METHODS get_message_text
        REDEFINITION .
    METHODS run
        REDEFINITION .

  PROTECTED SECTION.
*"* protected components of class CL_CI_TEST_FREE_SEARCH
*"* do not include other source files here!!!

  PRIVATE SECTION.

*"* private components of class CL_CI_TEST_FREE_SEARCH
*"* do not include other source files here!!!
    CONSTANTS c_my_name TYPE sci_chk VALUE 'ZIOT_CL_CI_TEST_TODO_FLAGS' ##NO_TEXT.

ENDCLASS.



CLASS ZIOT_CL_CI_CHECK_TODO_FLAGS IMPLEMENTATION.


  METHOD constructor .

    super->constructor( ).
    description = TEXT-000.

  ENDMETHOD.


  METHOD get_message_text .

    DATA: l_code TYPE sci_errc.

    IF p_test <> myname OR p_code CP '_*_'.
      super->get_message_text(
                  EXPORTING p_test = p_test p_code = p_code
                  IMPORTING p_text = p_text ).
      RETURN.
    ENDIF.

    l_code = p_code.
    SHIFT l_code LEFT DELETING LEADING space.
    p_text = '&1'.
    REPLACE FIRST OCCURRENCE OF '&N' IN p_text WITH l_code.

  ENDMETHOD.


  METHOD run.

    DATA: l_include       TYPE sobj_name,
          l_row           TYPE token_row,
          l_column        TYPE token_col,
          l_tokennr       LIKE statement_wa-from,
          l_code          TYPE sci_errc,
          l_search_string LIKE LINE OF search_strings,
          l_position      TYPE i,
          l_checksum      TYPE sci_crc64,
          l_comment       TYPE string.

    FIELD-SYMBOLS: <ls_comment_token> TYPE stokesx.

    IF search_strings IS INITIAL.
      RETURN.
    ENDIF.

    IF ref_scan IS INITIAL.
      CHECK get( ) = 'X'.
    ENDIF.

    CHECK ref_scan->subrc = 0.

*-- loop at all tokens
    LOOP AT ref_scan->statements INTO statement_wa.
      CHECK statement_wa-from <= statement_wa-to.
      l_position = sy-tabix.
      IF statement_wa-type = 'S' OR
         statement_wa-type = 'P'.
        CHECK comment_mode = 'X'.
      ENDIF.

      LOOP AT ref_scan->tokens INTO token_wa
             FROM statement_wa-from TO statement_wa-to.
        l_tokennr = sy-tabix.
        IF token_wa-type = 'S'.
          CHECK literal_mode = 'X'.
        ENDIF.

        LOOP AT search_strings INTO l_search_string.
*-- does ABAP-string contain search-string ?
          IF token_wa-str EQ l_search_string.
            UNPACK sy-tabix TO l_code(4).
            l_include = get_include( ).
            l_row     = get_line_abs( l_tokennr ).
            l_column  = get_column_abs( l_tokennr ).
            CLEAR l_checksum.
            cl_ci_provide_checksum=>gen_chksum_from_chars( EXPORTING p_param = l_search_string
                                                           CHANGING  p_crc_value = l_checksum
                                                           EXCEPTIONS parameter_error = 0 ).
            get_stmt_checksum( EXPORTING p_position = l_position p_version = 1
                               CHANGING p_checksum = l_checksum
                               EXCEPTIONS error = 1 ).

            ASSIGN ref_scan->tokens[ l_tokennr + 1 ] TO <ls_comment_token>.

            CLEAR: l_comment.
            IF <ls_comment_token> IS ASSIGNED.
              l_comment = <ls_comment_token>-str.
              UNASSIGN: <ls_comment_token>.
            ENDIF.

            inform( p_sub_obj_type = c_type_include
                    p_sub_obj_name = l_include
                    p_position     = l_position
                    p_line         = l_row
                    p_column       = l_column
                    p_kind         = msgkind
                    p_test         = c_my_name
                    p_code         = l_code
                    p_suppress     = 'CI_NOFIND'
                    p_param_1      = |{ token_wa-str } { l_comment }|
                    p_checksum_1   = l_checksum-i1 ).
            EXIT.
          ENDIF.     "l_strpos > l_pos
        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
