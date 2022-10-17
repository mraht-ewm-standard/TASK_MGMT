CLASS ziot_apack_task_mgmt DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_apack_manifest.

    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ziot_apack_task_mgmt IMPLEMENTATION.

  METHOD constructor.

    if_apack_manifest~descriptor-group_id    = 'c-a-s.de'.
    if_apack_manifest~descriptor-artifact_id = 'ewm-task-mgmt'.
    if_apack_manifest~descriptor-version     = '17.10.2022.001-rc'.
    if_apack_manifest~descriptor-git_url     = 'https://github.com/mraht-ewm-standard/TASK_MGMT.git'.

    if_apack_manifest~descriptor-dependencies = VALUE #( ( group_id       = 'c-a-s.de'
                                                            artifact_id    = 'ewm-dev-basis'
                                                            target_package = 'ZIOT_DEV_BASIS'
                                                            git_url        = 'https://github.com/mraht-ewm-standard/DEV_BASIS.git' ) ).

  ENDMETHOD.

ENDCLASS.
