@AbapCatalog.sqlViewName: 'ZIN_SO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'View Sales Order Header and Item'
define view zin_cds_basic_sales_order_data
  as select from vbak as salesHeader
    left outer join   vbap as salesItem on salesHeader.vbeln = salesItem.vbeln
{
  key salesHeader.vbeln as salesOrder,
      salesHeader.erdat,
      salesHeader.ernam,
      //      @Aggregation.default: #SUM
      salesItem.netwr,
      @Semantics.currencyCode: true
      salesItem.waerk,
      @Aggregation.default: #SUM
      @Aggregation.exception: #LAST
      @Aggregation.referenceElement:'salesOrder'
      @Semantics.quantity.unitOfMeasure: 'UOM'
      salesItem.kwmeng,
      @Semantics.unitOfMeasure: true
      salesItem.vrkme   as UOM
}
