CLASS zcl_ecv_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF cs_connection,
                 BEGIN OF type,
                   sandbox    TYPE zecv_e_server_type VALUE 'S',
                   production TYPE zecv_e_server_type VALUE 'P',
                 END OF type,
                 BEGIN OF methods,
                   get_data TYPE string VALUE 'evdata',
                 END OF methods,
               END OF cs_connection.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ecv_data IMPLEMENTATION.
ENDCLASS.
