*&---------------------------------------------------------------------*
*& Report ziot_r_task_board
*&---------------------------------------------------------------------*
*& Open tasks: Use '##TODO. " [author] <task description>' within source
*&             code to create a new open task. E.g.:
*&             ##TODO. " [MRaht] Implement exception handling
*&             You can also use ##NEW, ##OPT or ##FIX. Configurable via
*&             source code and TCode SCI. See also package ZIOT_CI_SCAN.
*&---------------------------------------------------------------------*
REPORT ziot_r_task_board.

DATA: ok_code TYPE sy-ucomm.

##TODO.
" - Show author in separate table column
" - Show tasks type in separate table column
" - Show tasks description in separate table column
" - Read task flags from check variant

CLASS lcl_appl DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF s_open_task,
             editor_link TYPE icon_d.
             INCLUDE     TYPE ziot_cl_package=>s_dev_obj.
           TYPES: END OF s_open_task,
           t_open_task TYPE TABLE OF s_open_task.

    CONSTANTS: mc_report_name     TYPE progname VALUE 'ZIOT_R_TASK_BOARD',
               mc_test_class_name TYPE clasname VALUE 'ZIOT_CL_CI_CHECK_TODO_FLAGS'.

    CONSTANTS: mc_dflt_check_variant TYPE sci_chkv VALUE 'DEFAULT'.

    CLASS-DATA: mo_alv_table    TYPE REF TO cl_salv_table,
                mv_main_package TYPE devclass,
                mt_packages     TYPE ziot_cl_package=>t_package,
                mt_open_tasks   TYPE t_open_task.

    CLASS-METHODS:
      execute,
      refresh
        IMPORTING
          iv_main_package TYPE devclass OPTIONAL.

  PRIVATE SECTION.
    CLASS-DATA: mt_todo_flags TYPE sci_srchstr.

    CLASS-METHODS:
      prepare,
      set_main_package,
      det_sub_packages,
      det_open_tasks,
      display_alv,
      prepare_alv
        RAISING
          cx_static_check,
      det_todo_flags.

ENDCLASS.


CLASS lcl_event_handler DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS on_refresh
                FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.
    CLASS-METHODS on_hotspot_click
        FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
        row
        column.
    CLASS-METHODS on_settings_click.

  PRIVATE SECTION.
    CLASS-METHODS on_editor_link_click
      IMPORTING
        is_open_task TYPE lcl_appl=>s_open_task.

ENDCLASS.


CLASS lcl_appl IMPLEMENTATION.

  METHOD execute.

    CHECK mo_alv_table IS INITIAL.

    det_todo_flags( ).

    prepare( ).

    display_alv( ).

  ENDMETHOD.


  METHOD prepare.

    set_main_package( ).

    det_sub_packages( ).

    det_open_tasks( ).

  ENDMETHOD.


  METHOD set_main_package.

    CLEAR: mt_packages.

    IF mv_main_package CO ' _0'.
      " Determine parent package initially
      SELECT SINGLE FROM tadir
        FIELDS devclass
        WHERE obj_name EQ @mc_report_name
        INTO @DATA(lv_devclass).
      DATA(lt_packages) = ziot_cl_package=>det_packages( iv_package        = lv_devclass
                                                         iv_hierarchy_mode = ziot_cl_package=>mc_hierarchy_mode-parents ).
      mv_main_package = VALUE #( lt_packages[ parent = space ]-package OPTIONAL ).
    ENDIF.

    SELECT SINGLE FROM tdevc
                  JOIN tdevct ON tdevc~devclass = tdevct~devclass
      FIELDS @abap_true
      WHERE tdevc~devclass EQ @mv_main_package
        AND tdevct~spras   EQ @sy-langu
      INTO @DATA(lv_exists).

    IF lv_exists EQ abap_false.
      DATA(lv_msg) = CONV string( TEXT-904 ).
      REPLACE '&1' IN lv_msg WITH mv_main_package.
      MESSAGE lv_msg  TYPE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD det_sub_packages.

    CHECK mv_main_package CN ' _0'.

    mt_packages = CORRESPONDING #( ziot_cl_package=>det_packages( iv_package        = mv_main_package
                                                                  iv_hierarchy_mode = ziot_cl_package=>mc_hierarchy_mode-children
                                                                  iv_incl_package   = abap_true ) ).

  ENDMETHOD.


  METHOD det_open_tasks.

    DATA(lt_r_package)  = VALUE rseloption( FOR <s_package> IN mt_packages
                                              ( sign = 'I' option = 'EQ' low = <s_package>-package ) ).
    " SCI only allows select option 'EQ', thus we have to change this to 'CP' with placeholders
    DATA(lt_r_incl_str) = VALUE rseloption( FOR <v_todo_flag> IN mt_todo_flags
                                              ( sign = 'I' option = 'CP' low = |*{ <v_todo_flag> }*| ) ).

    CLEAR: mt_open_tasks.
    mt_open_tasks = CORRESPONDING #( ziot_cl_package=>search( iv_incl_subpckg = abap_true
                                                              it_r_packages   = lt_r_package
                                                              it_r_incl_str   = lt_r_incl_str ) ).
    DELETE mt_open_tasks WHERE obj_name CS mc_report_name
                            OR obj_name CS mc_test_class_name.

    LOOP AT mt_open_tasks ASSIGNING FIELD-SYMBOL(<ls_open_task>).
      <ls_open_task>-editor_link = icon_change.
    ENDLOOP.

  ENDMETHOD.


  METHOD display_alv.

    TRY.
        prepare_alv( ).

      CATCH cx_root.
        MESSAGE TEXT-900 TYPE 'E'.

    ENDTRY.

  ENDMETHOD.


  METHOD prepare_alv.

    CLEAR: mo_alv_table.
    cl_salv_table=>factory( EXPORTING r_container  = cl_gui_container=>default_screen
                            IMPORTING r_salv_table = mo_alv_table
                            CHANGING  t_table      = mt_open_tasks ).

    DATA(lo_functions) = mo_alv_table->get_functions( ).
    lo_functions->set_all( abap_true ).
    lo_functions->add_function( name     = 'REFRESH'
                                icon     = CONV #( icon_refresh )
                                tooltip  = CONV #( TEXT-008 )
                                position = 1 ).

    DATA(lo_display) = mo_alv_table->get_display_settings( ).
    lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
    lo_display->set_list_header( TEXT-100 ).

    DATA(lo_selection) = mo_alv_table->get_selections( ).
    lo_selection->set_selection_mode( if_salv_c_selection_mode=>cell ).

    DATA: lo_column TYPE REF TO cl_salv_column_table.
    DATA(lo_columns) = mo_alv_table->get_columns( ).

    lo_column ?= lo_columns->get_column( 'EDITOR_LINK' ).
    lo_column->set_long_text( TEXT-007 ).
    lo_column->set_medium_text( TEXT-007 ).
    lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

    lo_column ?= lo_columns->get_column( 'IS_NEW_EDITOR' ).
    lo_column->set_long_text( TEXT-104 ).
    lo_column->set_medium_text( TEXT-104 ).
    lo_column->set_short_text( TEXT-114 ).
    lo_column->set_alignment( if_salv_c_alignment=>centered ).

    lo_column ?= lo_columns->get_column( 'LINE_TEXT' ).
    lo_column->set_long_text( TEXT-103 ).
    lo_column->set_medium_text( TEXT-103 ).

    lo_column ?= lo_columns->get_column( 'LINE' ).
    lo_column->set_long_text( TEXT-102 ).
    lo_column->set_medium_text( TEXT-102 ).
    lo_column->set_short_text( TEXT-102 ).

    lo_column ?= lo_columns->get_column( 'OBJ_NAME' ).
    lo_column->set_long_text( TEXT-101 ).
    lo_column->set_medium_text( TEXT-101 ).

    lo_column ?= lo_columns->get_column( 'DEVCLASS' ).
    lo_column->set_long_text( TEXT-002 ).
    lo_column->set_medium_text( TEXT-002 ).

    lo_columns->set_optimize( abap_true ).

    DATA: lo_events TYPE REF TO cl_salv_events_table.
    lo_events = mo_alv_table->get_event( ).
    SET HANDLER lcl_event_handler=>on_refresh FOR lo_events.
    SET HANDLER lcl_event_handler=>on_hotspot_click FOR lo_events.

    mo_alv_table->display( ).

  ENDMETHOD.


  METHOD refresh.

    IF iv_main_package CN ' _0'.
      lcl_appl=>mv_main_package = iv_main_package.
    ENDIF.

    prepare( ).
    mo_alv_table->refresh( ).

  ENDMETHOD.


  METHOD det_todo_flags.

    DATA: lo_check_variant TYPE REF TO cl_ci_checkvariant.

    CLEAR: mt_todo_flags.

    WHILE lo_check_variant IS INITIAL.

      DATA(lv_index) = sy-index.
      CASE lv_index.
        WHEN 1.
          " Try to determine user-specific default check variant
          DATA(lv_uname)              = sy-uname.
          DATA(lv_dflt_check_variant) = mc_dflt_check_variant.

        WHEN 2.
          " Try to determine system-wide default check variant
          lv_uname = space.

          cl_ci_checkvariant=>get_chkv_alter(
            EXPORTING
              p_checkvname_default = mc_dflt_check_variant
            IMPORTING
              p_checkvname_new     = lv_dflt_check_variant ).

        WHEN OTHERS.
          MESSAGE TEXT-905 TYPE 'E'.

      ENDCASE.

      cl_ci_checkvariant=>get_ref(
        EXPORTING
          p_user = lv_uname
          p_name = lv_dflt_check_variant
        RECEIVING
          p_ref  = lo_check_variant
        EXCEPTIONS
          OTHERS = 0 ).

    ENDWHILE.

    DATA(lv_check_variant_id) = CONV sci_cvkey( |{ lo_check_variant->chkvinf-checkvid }_| &&
                                                |{ lo_check_variant->chkvinf-ciuser }| ).

    DATA(variant) = VALUE sci_tstvar( ).
    IMPORT parameter = variant FROM DATABASE scichkv_pa(ar) ID lv_check_variant_id.
    DELETE variant WHERE testname NE mc_test_class_name.

    cl_ci_tests=>get_list(
      EXPORTING
        p_variant = variant
      RECEIVING
        p_result  = DATA(lo_test_ref)
      EXCEPTIONS
        OTHERS    = 0 ).

    DATA(lt_list) = lo_test_ref->give_list( ).
    ASSIGN lt_list[ 1 ] TO FIELD-SYMBOL(<lo_test_class>).
    IF <lo_test_class> IS NOT ASSIGNED.
      MESSAGE TEXT-906 TYPE 'E'.
    ENDIF.

    DATA(lv_attributes) = <lo_test_class>->get_attributes( ).
    IMPORT search_strings = mt_todo_flags FROM DATA BUFFER lv_attributes.

    IF mt_todo_flags IS INITIAL.
      MESSAGE TEXT-907 TYPE 'E'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_refresh.

    lcl_appl=>refresh(  ).

  ENDMETHOD.


  METHOD on_hotspot_click.

    ASSIGN lcl_appl=>mt_open_tasks[ row ] TO FIELD-SYMBOL(<ls_open_task>).
    CHECK <ls_open_task> IS ASSIGNED.

    CASE column.
      WHEN 'EDITOR_LINK'.
        on_editor_link_click( <ls_open_task> ).

    ENDCASE.

  ENDMETHOD.


  METHOD on_editor_link_click.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation   = 'SHOW'
        object_name = is_open_task-obj_name
        object_type = 'REPS'
        position    = is_open_task-line
      EXCEPTIONS
        OTHERS      = 0.

  ENDMETHOD.


  METHOD on_settings_click.

    TYPES: tt_sval TYPE STANDARD TABLE OF sval WITH DEFAULT KEY.

    DATA(lt_fields) = VALUE tt_sval( ( tabname   = 'TADIR'
                                       fieldname = 'DEVCLASS'
                                       value     = lcl_appl=>mv_main_package ) ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title  = TEXT-010
        start_column = '2'
        start_row    = '2'
      TABLES
        fields       = lt_fields
      EXCEPTIONS
        OTHERS       = 0.

    ASSIGN lt_fields[ 1 ] TO FIELD-SYMBOL(<ls_field>).
    CHECK <ls_field> IS ASSIGNED.

    lcl_appl=>refresh( CONV #( <ls_field>-value ) ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  CALL SCREEN 0100.

MODULE pbo_0100 OUTPUT.

  SET PF-STATUS 'MAIN'.

  lcl_appl=>execute( ).

ENDMODULE.


MODULE pai_0100 INPUT.

  DATA(save_ok) = ok_code.
  CLEAR: ok_code.

  CASE save_ok.
    WHEN 'CHOOSE'.
      lcl_event_handler=>on_settings_click( ).

  ENDCASE.

ENDMODULE.
