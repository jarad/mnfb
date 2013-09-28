Put csv files exported from mnfb_database here. 
Current version of the the csv files sent to Jarad Niemi (niemi@iastate.edu) from Ed Zlonis (zloni011@d.umn.edu) on 24 September 2013. 
Exported by Jarad Niemi on 28 September 2013 using the following visual basic script: 

    Option Compare Database 
    Public Sub ExportAll() 
        Dim obj As AccessObject, dbs As Object 
        Set dbs = Application.CurrentData 
        For Each obj In dbs.AllTables 
            If Left(obj.Name, 4) <> "MSys" Then 
                DoCmd.TransferText acExportDelim, , obj.Name, obj.Name & ".csv", True 
            End If 
        Next obj 
    End Sub

and then copying and pasting to the appropriate directory. 

NFB_Site_Origin.csv received from Ed Zlonis on Jul 21, 2013.

