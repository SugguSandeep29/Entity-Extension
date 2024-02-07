@AccessControl.authorizationCheck: #NOT_ALLOWED
@EndUserText.label: 'Validation of fields on MDG UI'
@Metadata.allowExtensions: true
define root view entity ZIN_DD_ATTR_VALIDATION as select from zin_t_bp_rule

{
key attribute,
    entity,
    mandatory
}
