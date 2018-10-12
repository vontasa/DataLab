# Setup an ODBC connection to SQLserver and grab the data by query
library(RODBC);
myconn <-odbcConnect("PDOperations_SQL", uid="corp_ops_01", pwd="corpops1"); # ODBC connection




customer<-sqlQuery(myconn,"
  USE PSC;
SELECT TOP 1000 a.fdc
      ,a.shiftStartDate
      ,a.ToteCaseID
      ,a.RouteNumber
      ,c.truck
      ,a.[SortZone]
      ,b.ZNDESC
      ,b.Storage
      ,a.[lines]
      ,a.[picks]
      ,a.[extNifo]
      ,a.[productCubicInches]
      ,a.totes
      ,a.[cases]
      ,g.toteSize
      ,a.productCubicInches/g.toteSize as utilization
      ,CustomerType = 
            Case 
                  When d.PRIM_DCSN_MKR_NUM=143 and (d.Accounting_Class!=30 or d.Accounting_Class!=43)then 'WAGs' 
                  Else 'Not WAGs'
            End
  FROM dbo.ToteCaseHistory a left join
  dbo.dtk_fpzonmas b on 
  (a.fdc=b.fdc and a.SortZone = b.ZNZONE)
  left join RouteLinehaulXrefMaster c on
  (a.fdc=c.fdc and a.RouteNumber=c.RouteNumber)
  left join dbo.pdCustomerMaster d on
  (a.fdc=d.CustomerDC and a.CustomerNumber=d.CustomerNumber)
  left join dbo.LinehaulMaster e on
  (a.fdc=e.fdc and c.Truck=e.Truck)
  left join dbo.EquipmentTypesMaster f on
  (e.EquipType=f.EquipType)
  left join dbo.dtk_fpzonbat g on
  (a.fdc=g.fdc and a.SortZone=g.sortZone)
  where a.productCubicInches >0 and (a.shiftStartDate BETWEEN '2013-5-1' AND '2013-5-1') and a.totes=1 and (a.fdc=3 or a.fdc=11);
                      ");

str(customer)
tempTable<-table(CustomerDC, MKT_SGMNT_DESC, CombineOrders, ProdTypeSplit)
customerFlag<-as.data.frame(tempTable)
result<-write.csv(customerFlag, "C:\\Users\\edward.wang\\Documents\\Project\\Tote Reduction Analysis\\customerFlag.csv")
