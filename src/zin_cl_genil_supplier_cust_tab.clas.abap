class ZIN_CL_GENIL_SUPPLIER_CUST_TAB definition
  public
  inheriting from CL_BS_GENIL_CUSTOMER
  final
  create public .

public section.

  methods IF_GENIL_APPL_DYN_META_INFO~IS_CHILD_CREATE_ALLOWED
    redefinition .
protected section.

  methods TRANSFORM_TO_ENTITY_KEY
    redefinition .
private section.
ENDCLASS.



CLASS ZIN_CL_GENIL_SUPPLIER_CUST_TAB IMPLEMENTATION.


  METHOD if_genil_appl_dyn_meta_info~is_child_create_allowed.
*** This method controls if the creation of a supplier dependend object is allowed or not
* delegate to parent first
    super->if_genil_appl_dyn_meta_info~is_child_create_allowed(
      EXPORTING
        iv_relation_name     = iv_relation_name                 " Relation Name
        iv_relation_filter   = iv_relation_filter                 " Relation Filter Interface
        iv_child_name        = iv_child_name                 " External Name of Object
      CHANGING
        cv_parent_and_result = cv_parent_and_result                 " Object Instance with Boolean Result
    ).

    CASE iv_child_name.
      WHEN 'ZBS_SP_SECRET_NUMBERS'.
        cv_parent_and_result-success = abap_true. "It's always allowed
      WHEN OTHERS.

    ENDCASE.
  ENDMETHOD.


  METHOD transform_to_entity_key.

*** This method transforms a GenIL object ID into its entity key.
*** It requires the correct key structure in the chancing parameter.
    DATA:
      lr_genil_key TYPE REF TO data,
      lv_structure TYPE string.

    FIELD-SYMBOLS:
      <ls_genil_key> TYPE key,
      <lv_bp_id>     TYPE any,
      <lv_bp_guid>   TYPE any.
* limit handling to supplier objects
    CASE iv_object_name.
      WHEN 'ZBS_SP_SECRET_NUMBERS'.
* get GenIL key
        TRY.
            lv_structure = me->object_model->get_key_struct_name( iv_object_name = iv_object_name ).
          CATCH cx_crm_unsupported_object.
            RETURN
            .ENDTRY.
        CREATE DATA lr_genil_key TYPE (lv_structure).
        ASSIGN lr_genil_key->* TO <ls_genil_key>.
        me->transform_to_object_key(
        EXPORTING
        iv_object_name = iv_object_name
        iv_object_id = iv_object_id
        IMPORTING
        es_object_key = <ls_genil_key> ).
* map to entity key
        me->mo_typecasting->map(
        EXPORTING
        is_source_structure = <ls_genil_key>
        CHANGING
        cs_target_structure = cs_entity_key ).
* BP ID requires special logic
        ASSIGN COMPONENT 'BP_HEADER' OF STRUCTURE cs_entity_key TO <lv_bp_id>.
        ASSIGN COMPONENT 'BP_GUID' OF STRUCTURE <ls_genil_key> TO <lv_bp_guid>.
        CHECK <lv_bp_id> IS ASSIGNED AND <lv_bp_guid> IS ASSIGNED.
        <lv_bp_id> = me->get_bp_id( iv_bp_guid = <lv_bp_guid> ).
      WHEN OTHERS.
* call parent

        CALL METHOD super->transform_to_entity_key
          EXPORTING
            iv_object_name = iv_object_name
            iv_object_id   = iv_object_id
          CHANGING
            cs_entity_key  = cs_entity_key.
       ENDCASE.
      ENDMETHOD.
ENDCLASS.
