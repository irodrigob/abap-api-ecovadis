*&---------------------------------------------------------------------*
*& Report ZECV_GET_INTEGRATIONS_ID
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zecv_get_integrations_id.

TABLES: lfa1.
DATA: lv_ecovadis_id TYPE text50.

PARAMETERS: p_type TYPE zecv_t001-type OBLIGATORY.

SELECTION-SCREEN SKIP 1.

PARAMETERS: p_lifnr RADIOBUTTON GROUP sel USER-COMMAND radio DEFAULT 'X',
            p_evid  RADIOBUTTON GROUP sel.

SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS s_lifnr FOR lfa1-lifnr NO INTERVALS MODIF ID lif.
SELECT-OPTIONS s_evid FOR lv_ecovadis_id NO INTERVALS MODIF ID evi.


AT SELECTION-SCREEN OUTPUT.


  LOOP AT SCREEN.
    IF screen-group1 = 'LIF'.
      screen-active = COND #( WHEN p_lifnr EQ abap_true THEN '1' ELSE '0').
    ENDIF.
    IF screen-group1 = 'EVI'.
      screen-active = COND #( WHEN p_evid EQ abap_true THEN '1' ELSE '0').
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.


START-OF-SELECTION.


  DATA(lo_data) = NEW zcl_ecv_api_downstream( iv_type = p_type ).

  TRY.

      CASE abap_true.
        WHEN p_lifnr.
          DATA(lt_data) = lo_data->get_data_from_integrations_id( it_integrations_id = VALUE #( FOR <wa> IN s_lifnr ( |{ <wa>-low ALPHA = OUT }| ) ) ).
        WHEN p_evid.
          lt_data = lo_data->get_data_from_ecovadis_ids( it_ecovadis_ids = VALUE #( FOR <a> IN s_evid ( |{ <a>-low  }| ) ) ).
      ENDCASE.

      IF  lt_data IS INITIAL.
        cl_demo_output=>write_text( text = 'No se ha encontrado ningun registro' ).
        cl_demo_output=>display( ).
        EXIT.
      ENDIF.

      cl_demo_output=>display_data( zcl_ecv_api_downstream=>convert_response_data_to_table( lt_data ) ).

    CATCH zcx_ecv INTO DATA(lo_excep).
      cl_demo_output=>write_text( text = lo_excep->get_text( ) ).
      cl_demo_output=>display( ).

  ENDTRY.
