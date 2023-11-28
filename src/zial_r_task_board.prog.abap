*&---------------------------------------------------------------------*
*& Report zial_r_task_board
*&---------------------------------------------------------------------*
*& Open tasks: Use '##TODO. " @<author> <task description>' within source
*&             code to create a new open task. E.g.:
*&             ##TODO. " @JDoe Implement exception handling
*&             You can also use ##NEW, ##OPT or ##FIX. Configurable via
*&             source code and TCode SCI. See also package ZIAL_CI_SCAN.
*&---------------------------------------------------------------------*
REPORT zial_r_task_board.

INCLUDE zial_r_task_board_top.
INCLUDE zial_r_task_board_cls.
INCLUDE zial_r_task_board_mod.

START-OF-SELECTION.
  CALL SCREEN 0100.
