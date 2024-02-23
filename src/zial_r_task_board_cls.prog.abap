*&---------------------------------------------------------------------*
*& Include zial_r_task_board_cls
*&---------------------------------------------------------------------*

CLASS lcl_application DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF s_dev_obj,
             is_new_editor TYPE abap_bool,
             task_type     TYPE char4,
             assignee      TYPE uname,
             line_text     TYPE char120,
             line          TYPE numc4,
             obj_name      TYPE sobj_name,
             devclass      TYPE devclass,
           END OF s_dev_obj,
           tt_dev_obj TYPE TABLE OF s_dev_obj WITH KEY obj_name.

    TYPES: BEGIN OF s_open_task,
             editor_link TYPE icon_d.
             INCLUDE     TYPE s_dev_obj.
    TYPES: END OF s_open_task,
           tt_open_task TYPE TABLE OF s_open_task.

    CONSTANTS mc_report_name        TYPE progname VALUE 'ZIAL_R_TASK_BOARD'.
    CONSTANTS mc_test_class_name    TYPE clasname VALUE 'ZIAL_CL_CI_TEST_TODO_FLAGS'.

    CONSTANTS mc_dflt_check_variant TYPE sci_chkv VALUE 'DEFAULT'.

    CLASS-DATA mo_alv_table    TYPE REF TO cl_salv_table.
    CLASS-DATA mv_main_package TYPE devclass.
    CLASS-DATA mt_packages     TYPE zial_tt_package.
    CLASS-DATA mt_open_tasks   TYPE tt_open_task.

    CLASS-METHODS execute.

    CLASS-METHODS refresh
      IMPORTING iv_main_package TYPE devclass OPTIONAL.

  PRIVATE SECTION.
    CLASS-DATA mt_todo_flags TYPE sci_srchstr.

    CLASS-METHODS prepare.
    CLASS-METHODS set_main_package.
    CLASS-METHODS det_sub_packages.
    CLASS-METHODS det_open_tasks.
    CLASS-METHODS display_alv.

    CLASS-METHODS prepare_alv
      RAISING cx_static_check.

    CLASS-METHODS det_todo_flags.

    CLASS-METHODS scan
      IMPORTING it_r_search_str  TYPE rseloption
                it_dev_objects   TYPE zial_tt_tadir
      RETURNING VALUE(rt_result) TYPE tt_dev_obj.

    CLASS-METHODS search
      IMPORTING iv_incl_subpckg  TYPE abap_bool  DEFAULT abap_true
                it_r_package     TYPE rseloption
                it_r_search_str  TYPE rseloption
                it_r_obj_name    TYPE rseloption OPTIONAL
      RETURNING VALUE(rt_result) TYPE tt_dev_obj.

ENDCLASS.


CLASS lcl_event_handler DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS on_refresh
      FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.

    CLASS-METHODS on_hotspot_click
      FOR EVENT link_click OF cl_salv_events_table
      IMPORTING !row
                !column.

    CLASS-METHODS on_settings_click.

  PRIVATE SECTION.
    CLASS-METHODS on_editor_link_click
      IMPORTING is_open_task TYPE lcl_application=>s_open_task.

ENDCLASS.


CLASS lcl_application IMPLEMENTATION.

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

    CLEAR mt_packages.

    IF mv_main_package CO ' _0'.
      " Determine parent package initially
      SELECT SINGLE FROM tadir
        FIELDS devclass
        WHERE pgmid    EQ 'R3TR'
          AND object   EQ 'PROG'
          AND obj_name EQ @mc_report_name
        INTO @DATA(lv_devclass).
      DATA(lt_packages) = zial_cl_dev_obj=>det_packages(
                              iv_package        = lv_devclass
                              iv_hierarchy_mode = zial_cl_dev_obj=>mc_hierarchy_mode-parents ).
      mv_main_package = VALUE #( lt_packages[ parent = space ]-package OPTIONAL ).
    ENDIF.

    SELECT SINGLE
      FROM tdevc
      JOIN tdevct ON tdevc~devclass EQ tdevct~devclass
      FIELDS @abap_true
      WHERE tdevc~devclass EQ @mv_main_package
      INTO @DATA(lv_exists).                           "#EC CI_BUFFJOIN

    IF lv_exists EQ abap_false.
      DATA(lv_msg) = CONV string( TEXT-904 ).
      REPLACE '&1' IN lv_msg WITH mv_main_package.
      MESSAGE lv_msg  TYPE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD det_sub_packages.

    CHECK mv_main_package CN ' _0'.

    mt_packages = CORRESPONDING #( zial_cl_dev_obj=>det_packages(
                                       iv_package        = mv_main_package
                                       iv_hierarchy_mode = zial_cl_dev_obj=>mc_hierarchy_mode-children
                                       iv_incl_package   = abap_true ) ).

  ENDMETHOD.


  METHOD det_open_tasks.

    DATA(lt_r_package) = VALUE rseloption( FOR <s_package> IN mt_packages
                                           ( sign = 'I' option = 'EQ' low = <s_package>-package ) ).
    " SCI only allows select option 'EQ', thus we have to change this to 'CP' with placeholders
    DATA(lt_r_search_str) = VALUE rseloption( FOR <v_todo_flag> IN mt_todo_flags
                                              ( sign = 'I' option = 'CP' low = |*{ <v_todo_flag> }*| )
                                              ( sign = 'E' option = 'NP' low = '*' ) ).
    DATA(lt_r_obj_name) = VALUE rseloption( ( sign = 'E' option = 'CP' low = |*{ mc_report_name }*| )
                                            ( sign = 'E' option = 'CP' low = |*{ mc_test_class_name }*| ) ).

    CLEAR mt_open_tasks.
    mt_open_tasks = CORRESPONDING #( search( iv_incl_subpckg = abap_true
                                             it_r_package    = lt_r_package
                                             it_r_search_str = lt_r_search_str
                                             it_r_obj_name   = lt_r_obj_name ) ).

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

    DATA lo_column TYPE REF TO cl_salv_column_table.
    DATA lo_events TYPE REF TO cl_salv_events_table.

    CLEAR mo_alv_table.
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

    lo_column ?= lo_columns->get_column( 'TASK_TYPE' ).
    lo_column->set_long_text( TEXT-105 ).
    lo_column->set_medium_text( TEXT-105 ).

    lo_column ?= lo_columns->get_column( 'ASSIGNEE' ).
    lo_column->set_long_text( TEXT-106 ).
    lo_column->set_medium_text( TEXT-106 ).

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

    lo_events = mo_alv_table->get_event( ).
    SET HANDLER lcl_event_handler=>on_refresh FOR lo_events.
    SET HANDLER lcl_event_handler=>on_hotspot_click FOR lo_events.

    mo_alv_table->display( ).

  ENDMETHOD.


  METHOD refresh.

    IF iv_main_package CN ' _0'.
      mv_main_package = iv_main_package.
    ENDIF.

    prepare( ).
    mo_alv_table->refresh( ).

  ENDMETHOD.


  METHOD det_todo_flags.

    DATA lo_check_variant TYPE REF TO cl_ci_checkvariant.

    CLEAR mt_todo_flags.

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

          cl_ci_checkvariant=>get_chkv_alter( EXPORTING p_checkvname_default = mc_dflt_check_variant
                                              IMPORTING p_checkvname_new     = lv_dflt_check_variant ).

        WHEN OTHERS.
          MESSAGE TEXT-905 TYPE 'E'.

      ENDCASE.

      cl_ci_checkvariant=>get_ref( EXPORTING  p_user = lv_uname
                                              p_name = lv_dflt_check_variant
                                   RECEIVING  p_ref  = lo_check_variant
                                   EXCEPTIONS OTHERS = 0 ).

    ENDWHILE.

    DATA(lv_check_variant_id) = CONV sci_cvkey( |{ lo_check_variant->chkvinf-checkvid }_| &&
                                                |{ lo_check_variant->chkvinf-ciuser }| ).

    DATA(lv_variant) = VALUE sci_tstvar( ).
    IMPORT parameter = lv_variant FROM DATABASE scichkv_pa(ar) ID lv_check_variant_id.
    lv_variant = FILTER #( lv_variant WHERE testname EQ CONV #( mc_test_class_name ) ).

    cl_ci_tests=>get_list( EXPORTING  p_variant = lv_variant
                           RECEIVING  p_result  = DATA(lo_test_ref)
                           EXCEPTIONS OTHERS    = 0 ).

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


  METHOD scan.

    LOOP AT it_dev_objects ASSIGNING FIELD-SYMBOL(<ls_dev_object>).

      cl_progress_indicator=>progress_indicate( i_text               = |\|&1% - { TEXT-115 } &4 (&2/&3)\||
                                                i_processed          = sy-tabix
                                                i_total              = lines( it_dev_objects )
                                                i_msgv4              = <ls_dev_object>-obj_name
                                                i_output_immediately = abap_true ).

      DATA(lt_source_code) = VALUE tchar255( ).
      READ REPORT <ls_dev_object>-obj_name INTO lt_source_code STATE 'I'.
      IF sy-subrc NE 0.
        READ REPORT <ls_dev_object>-obj_name INTO lt_source_code.
      ENDIF.

      CHECK lt_source_code IS NOT INITIAL.

      LOOP AT lt_source_code ASSIGNING FIELD-SYMBOL(<lv_source_code>).

        DATA(lv_line) = sy-tabix.

        CHECK <lv_source_code> IN it_r_search_str.

        APPEND INITIAL LINE TO rt_result ASSIGNING FIELD-SYMBOL(<ls_result>).
        <ls_result>           = CORRESPONDING #( <ls_dev_object> ).
        <ls_result>-line      = lv_line.
        <ls_result>-line_text = <lv_source_code>.
        SHIFT <ls_result>-line_text LEFT DELETING LEADING space.

        IF <ls_dev_object>-obj_name CP '*=CS'.
          <ls_result>-is_new_editor = abap_true.
        ENDIF.

        TRY.
            DATA(lo_matcher) = cl_abap_matcher=>create( pattern = |##([A-Z]+\\.?)(?:\\s+"\\s*@(\\w+))?|
                                                        text    = <lv_source_code> ).
            IF lo_matcher->find_next( ) EQ abap_true.
              <ls_result>-task_type = lo_matcher->get_submatch( 1 ).
              <ls_result>-assignee  = lo_matcher->get_submatch( 2 ).
            ENDIF.

          CATCH cx_root.
            CONTINUE.

        ENDTRY.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD search.

    CHECK it_r_package    IS NOT INITIAL
      AND it_r_search_str IS NOT INITIAL.

    DATA(lt_r_package) = it_r_package.
    IF iv_incl_subpckg EQ abap_true.
      LOOP AT it_r_package ASSIGNING FIELD-SYMBOL(<ls_r_package>).
        DATA(lt_packages) = zial_cl_dev_obj=>det_packages( iv_package = CONV #( <ls_r_package>-low ) ).
        APPEND LINES OF VALUE rseloption( FOR <s_package> IN lt_packages
                                          ( sign   = 'I'
                                            option = 'EQ'
                                            low    = <s_package>-package ) ) TO lt_r_package.
      ENDLOOP.
    ENDIF.

    DATA(lt_r_obj_type) = VALUE rseloption( ( sign = 'I' option = 'EQ' low = zial_cl_dev_obj=>mc_dev_obj_type-clas )
                                            ( sign = 'I' option = 'EQ' low = zial_cl_dev_obj=>mc_dev_obj_type-fugr )
                                            ( sign = 'I' option = 'EQ' low = zial_cl_dev_obj=>mc_dev_obj_type-intf )
                                            ( sign = 'I' option = 'EQ' low = zial_cl_dev_obj=>mc_dev_obj_type-prog ) ).
    DATA(lt_dev_objects) = zial_cl_dev_obj=>det_by_package( iv_with_includes = abap_true
                                                            iv_excl_base_obj = abap_true
                                                            it_r_package     = lt_r_package
                                                            it_r_obj_type    = lt_r_obj_type
                                                            it_r_obj_name    = it_r_obj_name ).

    rt_result = scan( it_dev_objects  = lt_dev_objects
                      it_r_search_str = it_r_search_str ).

  ENDMETHOD.

ENDCLASS.


CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_refresh.

    lcl_application=>refresh( ).

  ENDMETHOD.


  METHOD on_hotspot_click.

    ASSIGN lcl_application=>mt_open_tasks[ row ] TO FIELD-SYMBOL(<ls_open_task>).
    CHECK <ls_open_task> IS ASSIGNED.

    IF column EQ 'EDITOR_LINK'.
      on_editor_link_click( <ls_open_task> ).
    ENDIF.

  ENDMETHOD.


  METHOD on_editor_link_click.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING  operation   = 'SHOW'
                 object_name = is_open_task-obj_name
                 object_type = 'REPS'
                 position    = is_open_task-line
      EXCEPTIONS OTHERS      = 0.

  ENDMETHOD.


  METHOD on_settings_click.

    TYPES tt_sval TYPE STANDARD TABLE OF sval WITH DEFAULT KEY.

    DATA(lt_fields) = VALUE tt_sval( ( tabname   = 'TADIR'
                                       fieldname = 'DEVCLASS'
                                       value     = lcl_application=>mv_main_package ) ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING  popup_title  = TEXT-010
                 start_column = '2'
                 start_row    = '2'
      TABLES     fields       = lt_fields
      EXCEPTIONS OTHERS       = 0.

    ASSIGN lt_fields[ 1 ] TO FIELD-SYMBOL(<ls_field>).
    CHECK <ls_field> IS ASSIGNED.

    lcl_application=>refresh( CONV #( <ls_field>-value ) ).

  ENDMETHOD.

ENDCLASS.
