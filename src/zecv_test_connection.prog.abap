*&---------------------------------------------------------------------*
*& Report ZECV_TEST_CONNECTION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zecv_test_connection.

PARAMETERS: p_type TYPE zecv_t001-type OBLIGATORY.

START-OF-SELECTION.


  DATA(lo_ecv) = NEW zcl_ecv_connection( iv_type = p_type ).

  TRY.
      lo_ecv->connect( ).
      lo_ecv->generate_token( ).

      WRITE:/ 'Token: ', lo_ecv->mv_token_authentification.

    CATCH zcx_ecv INTO DATA(lo_excep).

      WRITE:/ lo_excep->get_text( ).

  ENDTRY.
