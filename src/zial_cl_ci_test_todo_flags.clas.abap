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
"!  <li>TCode 'ATC': Change global check variant → Save
"! </ol>
"! </p>
CLASS zial_cl_ci_test_todo_flags DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_test_free_search
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS get_message_text REDEFINITION.
    METHODS run              REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS mc_my_name TYPE sci_chk VALUE 'ZIAL_CL_CI_TEST_TODO_FLAGS' ##NO_TEXT.

ENDCLASS.


CLASS zial_cl_ci_test_todo_flags IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).
    description = TEXT-000.

  ENDMETHOD.


  METHOD get_message_text.

    IF p_test NE myname OR p_code CP '_*_'.
      super->get_message_text( EXPORTING p_test = p_test
                                         p_code = p_code
                               IMPORTING p_text = p_text ).
      RETURN.
    ENDIF.

    DATA(lv_code) = CONV sci_errc( p_code ).
    SHIFT lv_code LEFT DELETING LEADING space.
    p_text = '&1'.
    REPLACE FIRST OCCURRENCE OF '&N' IN p_text WITH lv_code.

  ENDMETHOD.


  METHOD run.

    DATA lv_include       TYPE sobj_name.
    DATA lv_row           TYPE token_row.
    DATA lv_column        TYPE token_col.
    DATA lv_tokennr       LIKE statement_wa-from.
    DATA lv_code          TYPE sci_errc.
    DATA lv_search_string LIKE LINE OF search_strings.
    DATA lv_position      TYPE i.
    DATA lv_checksum      TYPE sci_crc64.
    DATA lv_comment       TYPE string.

    FIELD-SYMBOLS <ls_comment_token> TYPE stokesx.

    IF search_strings IS INITIAL.
      RETURN.
    ENDIF.

    IF ref_scan IS INITIAL.
      CHECK get( ) EQ 'X'.
    ENDIF.

    CHECK ref_scan->subrc EQ 0.

    " -- loop at all tokens
    LOOP AT ref_scan->statements INTO statement_wa.
      CHECK statement_wa-from LE statement_wa-to.
      lv_position = sy-tabix.
      IF    statement_wa-type EQ 'S'
         OR statement_wa-type EQ 'P'.
        CHECK comment_mode EQ 'X'.
      ENDIF.

      LOOP AT ref_scan->tokens INTO token_wa FROM statement_wa-from TO statement_wa-to.
        lv_tokennr = sy-tabix.
        IF token_wa-type EQ 'S'.
          CHECK literal_mode EQ 'X'.
        ENDIF.

        LOOP AT search_strings INTO lv_search_string.
          " -- does ABAP-string contain search-string ?
          IF token_wa-str EQ lv_search_string.
            UNPACK sy-tabix TO lv_code(4).
            lv_include = get_include( ).
            lv_row     = get_line_abs( lv_tokennr ).
            lv_column  = get_column_abs( lv_tokennr ).
            CLEAR lv_checksum.
            cl_ci_provide_checksum=>gen_chksum_from_chars( EXPORTING  p_param         = lv_search_string
                                                           CHANGING   p_crc_value     = lv_checksum
                                                           EXCEPTIONS parameter_error = 0 ).
            get_stmt_checksum( EXPORTING  p_position = lv_position
                                          p_version  = 1
                               CHANGING   p_checksum = lv_checksum
                               EXCEPTIONS error      = 1 ).

            ASSIGN ref_scan->tokens[ lv_tokennr + 1 ] TO <ls_comment_token>.

            CLEAR lv_comment.
            IF <ls_comment_token> IS ASSIGNED.
              lv_comment = <ls_comment_token>-str.
              UNASSIGN <ls_comment_token>.
            ENDIF.

            inform( p_sub_obj_type = c_type_include
                    p_sub_obj_name = lv_include
                    p_position     = lv_position
                    p_line         = lv_row
                    p_column       = lv_column
                    p_kind         = msgkind
                    p_test         = mc_my_name
                    p_code         = lv_code
                    p_suppress     = 'CI_NOFIND'
                    p_param_1      = |{ token_wa-str } { lv_comment }|
                    p_checksum_1   = lv_checksum-i1 ).
            EXIT.
          ENDIF.     " l_strpos > l_pos
        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
