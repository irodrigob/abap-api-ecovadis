*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZECV_T000.......................................*
DATA:  BEGIN OF STATUS_ZECV_T000                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZECV_T000                     .
CONTROLS: TCTRL_ZECV_T000
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZECV_T001.......................................*
DATA:  BEGIN OF STATUS_ZECV_T001                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZECV_T001                     .
CONTROLS: TCTRL_ZECV_T001
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZECV_T002.......................................*
DATA:  BEGIN OF STATUS_ZECV_T002                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZECV_T002                     .
CONTROLS: TCTRL_ZECV_T002
            TYPE TABLEVIEW USING SCREEN '0003'.
*.........table declarations:.................................*
TABLES: *ZECV_T000                     .
TABLES: *ZECV_T001                     .
TABLES: *ZECV_T002                     .
TABLES: *ZECV_T002T                    .
TABLES: ZECV_T000                      .
TABLES: ZECV_T001                      .
TABLES: ZECV_T002                      .
TABLES: ZECV_T002T                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
