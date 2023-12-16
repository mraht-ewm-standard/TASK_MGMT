*&---------------------------------------------------------------------*
*& Include zial_r_task_board_mod
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

  SET PF-STATUS 'MAIN'.

  lcl_application=>execute( ).

ENDMODULE.

MODULE pai_0100 INPUT.

  DATA(save_ok) = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'CHOOSE'.
      lcl_event_handler=>on_settings_click( ).

    WHEN OTHERS.
      RETURN.

  ENDCASE.

ENDMODULE.

MODULE exit INPUT.

  LEAVE PROGRAM.

ENDMODULE.
