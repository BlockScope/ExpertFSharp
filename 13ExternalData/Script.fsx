open System.IO
open System.Net

let http (url : string) = 
    let req = WebRequest.Create(url)
    use resp = req.GetResponse()
    use stream = resp.GetResponseStream()
    let reader = new StreamReader(stream)
    reader.ReadToEnd()
//val http : url:string -> string

let worldBankCountriesXmlPage1 = http "http://api.worldbank.org/country"
//val worldBankCountriesXmlPage1 : string =
//  "<?xml version="1.0" encoding="utf-8"?>
//<wb:countries page="1"+[24134 chars]

worldBankCountriesXmlPage1
//val it : string =
//  "<?xml version="1.0" encoding="utf-8"?>
//<wb:countries page="1" pages="6" per_page="50" total="264" xmlns:wb="http://www.worldbank.org">
//  <wb:country id="ABW">
//    <wb:iso2Code>AW</wb:iso2Code>
//    <wb:name>Aruba</wb:name>
//    <wb:region id="LCN">Latin America &amp; Caribbean (all income levels)</wb:region>
//    <wb:adminregion id="" />
//    <wb:incomeLevel id="NOC">High income: nonOECD</wb:incomeLevel>
//    <wb:lendingType id="LNX">Not classified</wb:lendingType>
//    <wb:capitalCity>Oranjestad</wb:capitalCity>
//    <wb:longitude>-70.0167</wb:longitude>
//    <wb:latitude>12.5167</wb:latitude>
//  </wb:country>
//  <wb:country id="AFG">
//    <wb:iso2Code>AF</wb:iso2Code>
//    <wb:name>Afghanistan</wb:name>
//    <wb:region id="SAS">South Asia</wb:region>
//    <wb:adminregion id="SAS">South Asia</wb:adminregion>
//    <wb:incomeLevel id="LIC">Low income</wb:incomeLevel>
//    <wb:lendingType id="IDX">IDA</wb:lendingType>
//    <wb:capitalCity>Kabul</wb:capitalCity>
//    <wb:longitude>69.1761</wb:longitude>
//    <wb:latitude>34.5228</wb:latitude>
//  </wb:country>
//  ...
//  <wb:country id="COG">
//    <wb:iso2Code>CG</wb:iso2Code>
//    <wb:name>Congo, Rep.</wb:name>
//    <wb:region id="SSF">Sub-Saharan Africa (all income levels)</wb:region>
//    <wb:adminregion id="SSA">Sub-Saharan Africa (developing only)</wb:adminregion>
//    <wb:incomeLevel id="LMC">Lower middle income</wb:incomeLevel>
//    <wb:lendingType id="IDB">Blend</wb:lendingType>
//    <wb:capitalCity>Brazzaville</wb:capitalCity>
//    <wb:longitude>15.2662</wb:longitude>
//    <wb:latitude>-4.2767</wb:latitude>
//  </wb:country>
//</wb:countries>"

#I @"./packages/FSharp.Data/lib/net40"
#r "FSharp.Data.dll"
open FSharp.Data

type CountriesXml = XmlProvider<"http://api.worldbank.org/country">
let sampleCountries = CountriesXml.GetSample()
//--> Added 'C:\...\13ExternalData\./packages/FSharp.Data/lib/net40' to library include path
//--> Referenced 'C:\...\13ExternalData\./packages/FSharp.Data/lib/net40\FSharp.Data.dll'
//type CountriesXml = FSharp.Data.XmlProvider<...>
//val sampleCountries : FSharp.Data.XmlProvider<...>.Countries =
//  <wb:countries page="1" pages="6" per_page="50" total="264" xmlns:wb="http://www.worldbank.org">
//  <wb:country id="ABW">
//  </wb:country>
//</wb:countries>

sampleCountries.Countries.Length
//val it : int = 50

sampleCountries.Countries.[0].Name
//val it : string = "Aruba"

let worldBankCountriesJsonPage1 = http "http://api.worldbank.org/country?format=json"
//val worldBankCountriesJsonPage1 : string =
//  "[{"page":1,"pages":6,"per_page":"50","total":264},[{"id":"ABW"+[16726 chars]

type CountriesJson = JsonProvider<"http://api.worldbank.org/country?format=json">
let sampleCountriesFromJson = CountriesJson.GetSample()
//type CountriesJson = JsonProvider<...>
//val sampleCountriesFromJson : JsonProvider<...>.Root =
//  [
//  {
//    "page": 1,
//    "pages": 6,
//    "per_page": "50",
//    "total": 264
//  },
//  [
//    {
//      "id": "ABW",
//      "iso2Code": "AW",
//      "name": "Aruba",
//...
//    }
//  ]
//]
sampleCountriesFromJson.Array.Length
//val it : int = 50

sampleCountriesFromJson.Array.[0].Name
//val it : string = "Aruba"

// NOTE: To avoid the following error, add a reference to System.Xml.Linq.
//The type referenced through 'System.Xml.Linq.XElement' is defined in an assembly that is not referenced. You must add a reference to assembly 'System.Xml.Linq'.
#r "System.Xml.Linq.dll"

let worldBankCountriesXmlPage2 = http "http://api.worldbank.org/country?page=2"
let loadPageFromXml n = CountriesXml.Load("http://api.worldbank.org/country?page=" + string n)

let countries = 
    let page1 = loadPageFromXml 1
    [ for i in 1 .. page1.Pages do 
         let page = loadPageFromXml i
         yield! page.Countries ]

//--> Referenced 'C:\Windows\Microsoft.NET\Framework\v4.0.30319\System.Xml.Linq.dll'
//val worldBankCountriesXmlPage2 : string =
//  "<?xml version="1.0" encoding="utf-8"?>
//<wb:countries page="2"+[24311 chars]
//val loadPageFromXml : n:int -> XmlProvider<...>.Countries
//val countries : XmlProvider<...>.Country list =
//  [<wb:country id="ABW" xmlns:wb="http://www.worldbank.org">
//  <wb:iso2Code>AW</wb:iso2Code>
//  <wb:name>Aruba</wb:name>
//  ...
//  <wb:longitude />
//  <wb:latitude />
//</wb:country>;
//   ...]

countries.Length
//val it : int = 256

[ for c in countries -> c.Name ]
//val it : string list =
//  ["Aruba"; "Afghanistan"; "Africa"; "Angola"; "Albania"; "Andorra";
//   "Andean Region"; "Arab World"; "United Arab Emirates"; "Argentina";
//   "Armenia"; "American Samoa"; "Antigua and Barbuda"; "Australia"; "Austria";
//   "Azerbaijan"; "Burundi"; "Belgium"; "Benin"; "Burkina Faso"; "Bangladesh";
//   "Bulgaria"; "Bahrain"; "Bahamas, The"; "Bosnia and Herzegovina"; "Belarus";
//   "Belize"; "Bermuda"; "Bolivia"; "Brazil"; "Barbados"; "Brunei Darussalam";
//   "Bhutan"; "Botswana"; "Sub-Saharan Africa (IFC classification)";
//   "Central African Republic"; "Canada";
//   "East Asia and the Pacific (IFC classification)";
//   "Central Europe and the Baltics";
//   "Europe and Central Asia (IFC classification)"; "Switzerland";
//   "Channel Islands"; "Chile"; "China"; "Cote d'Ivoire";
//   "Latin America and the Caribbean (IFC classification)";
//   "Middle East and North Africa (IFC classification)"; "Cameroon";
//   "Congo, Dem. Rep."; "Congo, Rep."; "Colombia"; "Comoros"; "Cabo Verde";
//   "Costa Rica"; "South Asia (IFC classification)"; "Caribbean small states";
//   "Cuba"; "Curacao"; "Cayman Islands"; "Cyprus"; "Czech Republic"; "Germany";
//   "Djibouti"; "Dominica"; "Denmark"; "Dominican Republic"; "Algeria";
//   "East Asia & Pacific (developing only)";
//   "East Asia & Pacific (all income levels)";
//   "Europe & Central Asia (developing only)";
//   "Europe & Central Asia (all income levels)"; "Ecuador"; "Egypt, Arab Rep.";
//   "Euro area"; "Eritrea"; "Spain"; "Estonia"; "Ethiopia"; "European Union";
//   "Fragile and conflict affected situations"; "Finland"; "Fiji"; "France";
//   "Faeroe Islands"; "Micronesia, Fed. Sts."; "Gabon"; "United Kingdom";
//   "Georgia"; "Ghana"; "Guinea"; "Gambia, The"; "Guinea-Bissau";
//   "Equatorial Guinea"; "Greece"; "Grenada"; "Greenland"; "Guatemala"; "Guam";
//   "Guyana"; "High income"; ...]

let loadPageFromJson n = 
    CountriesJson.Load("http://api.worldbank.org/country?format=json&page=" + string n)

let countriesFromJson = 
    let page1 = loadPageFromJson 1
    [ for i in 1 .. page1.Record.Pages do 
         let page = loadPageFromJson i
         yield! (page.Array |> Seq.map (fun x -> x.Name)) ]
//val countriesFromJson : string list =
//  ["Aruba"; "Afghanistan"; "Africa"; "Angola"; "Albania"; "Andorra";
//   "Andean Region"; "Arab World"; "United Arab Emirates"; "Argentina";
//   "Armenia"; "American Samoa"; "Antigua and Barbuda"; "Australia"; "Austria";
//   "Azerbaijan"; "Burundi"; "Belgium"; "Benin"; "Burkina Faso"; "Bangladesh";
//   "Bulgaria"; "Bahrain"; "Bahamas, The"; "Bosnia and Herzegovina"; "Belarus";
//   "Belize"; "Bermuda"; "Bolivia"; "Brazil"; "Barbados"; "Brunei Darussalam";
//   "Bhutan"; "Botswana"; "Sub-Saharan Africa (IFC classification)";
//   "Central African Republic"; "Canada";
//   "East Asia and the Pacific (IFC classification)";
//   "Central Europe and the Baltics";
//   "Europe and Central Asia (IFC classification)"; "Switzerland";
//   "Channel Islands"; "Chile"; "China"; "Cote d'Ivoire";
//   "Latin America and the Caribbean (IFC classification)";
//   "Middle East and North Africa (IFC classification)"; "Cameroon";
//   "Congo, Dem. Rep."; "Congo, Rep."; "Colombia"; "Comoros"; "Cabo Verde";
//   "Costa Rica"; "South Asia (IFC classification)"; "Caribbean small states";
//   "Cuba"; "Curacao"; "Cayman Islands"; "Cyprus"; "Czech Republic"; "Germany";
//   "Djibouti"; "Dominica"; "Denmark"; "Dominican Republic"; "Algeria";
//   "East Asia & Pacific (developing only)";
//   "East Asia & Pacific (all income levels)";
//   "Europe & Central Asia (developing only)";
//   "Europe & Central Asia (all income levels)"; "Ecuador"; "Egypt, Arab Rep.";
//   "Euro area"; "Eritrea"; "Spain"; "Estonia"; "Ethiopia"; "European Union";
//   "Fragile and conflict affected situations"; "Finland"; "Fiji"; "France";
//   "Faeroe Islands"; "Micronesia, Fed. Sts."; "Gabon"; "United Kingdom";
//   "Georgia"; "Ghana"; "Guinea"; "Gambia, The"; "Guinea-Bissau";
//   "Equatorial Guinea"; "Greece"; "Grenada"; "Greenland"; "Guatemala"; "Guam";
//   "Guyana"; "High income"; ...]

let loadPageFromJson1000 = 
    CountriesJson.Load("http://api.worldbank.org/country?format=json&per_page=1000")
//val loadPageFromJson1000 : JsonProvider<...>.Root =
//  [
//  {
//    "page": 1,
//    "pages": 1,
//    "per_page": "1000",
//    "total": 264
//  },
//  [
//    {
//      "id": "ABW",
//      "iso2Code": "AW",
//      "name": "Aruba",
//      ...
//    },
//    ...
//    {
//      "id": "ZWE",
//      "iso2Code": "ZW",
//      "name": "Zimbabwe",
//      ...
//    }
//  ]
//]

#r "System.Data.Linq.dll"
#r "FSharp.Data.TypeProviders.dll"

open Microsoft.FSharp.Linq
open Microsoft.FSharp.Data.TypeProviders

type NorthwndDb = 
    SqlDataConnection<ConnectionString = 
                         @"AttachDBFileName  = 'C:\Scripts\northwnd.mdf';
                           Server='.\SQLEXPRESS';User Instance=true;Integrated Security=SSPI",
                      Pluralize=true>
//type NorthwndDb =
//  class
//    static member GetDataContext : unit -> NorthwndDb.ServiceTypes.SimpleDataContextTypes.Northwind
//     + 1 overload
//    nested type ServiceTypes
//  end

let db = NorthwndDb.GetDataContext()

let customersSortedByCountry = 
    query { for c in db.Customers do 
            sortBy c.Country
            select (c.Country, c.CompanyName) }
    |> Seq.toList
//val customersSortedByCountry : (string * string) list =
//  [("Argentina", "Cactus Comidas para llevar");
//   ("Argentina", "Océano Atlántico Ltda."); ("Argentina", "Rancho grande");
//   ...
//   ("Venezuela", "LINO-Delicateses"); ("Venezuela", "HILARION-Abastos");
//   ("Venezuela", "GROSELLA-Restaurante")]

db.DataContext.Log <- System.Console.Out
//SELECT [t0].[Country] AS [Item1], [t0].[ContactName] AS [Item2]
//FROM [dbo].[Customers] AS [t0]
//ORDER BY [t0].[Country]
//-- Context: SqlProvider(Sql2008) Model: AttributedMetaModel Build: 4.0.30319.17929

let selectedEmployees = 
    query { for emp in db.Employees do
            where (emp.BirthDate.Value.Year > 1960)
            where (emp.LastName.StartsWith "S")
            select (emp.FirstName, emp.LastName) 
            take 5 }
    |> Seq.toList
//SELECT TOP (5) [t0].[FirstName] AS [Item1], [t0].[LastName] AS [Item2]
//FROM [dbo].[Employees] AS [t0]
//WHERE ([t0].[LastName] LIKE @p0) AND (DATEPART(Year, [t0].[BirthDate]) > @p1)
//-- @p0: Input NVarChar (Size = 4000; Prec = 0; Scale = 0) [S%]
//-- @p1: Input Int (Size = -1; Prec = 0; Scale = 0) [1960]
//-- Context: SqlProvider(Sql2008) Model: AttributedMetaModel Build: 4.0.30319.17626

//val selectedEmployees : (string * string) list = [("Michael", "Suyama")]

let customersSortedTwoColumns = 
    query { for c in db.Customers do 
            sortBy c.Country
            thenBy c.Region
            select (c.Country, c.Region, c.CompanyName) }
    |> Seq.toList
//SELECT [t0].[Country] AS [Item1], [t0].[Region] AS [Item2], [t0].[CompanyName] AS [Item3]
//FROM [dbo].[Customers] AS [t0]
//ORDER BY [t0].[Country], [t0].[Region]
//-- Context: SqlProvider(Sql2008) Model: AttributedMetaModel Build: 4.0.30319.17626

//val customersSortedTwoColumns : (string * string * string) list =
//  [("Argentina", null, "Cactus Comidas para llevar");
//   ("Argentina", null, "Océano Atlántico Ltda.");
//   ("Argentina", null, "Rancho grande"); ("Austria", null, "Piccolo und mehr");
//   ("Austria", null, "Ernst Handel"); ("Belgium", null, "Maison Dewey");
//   ...
//   ("Venezuela", "DF", "GROSELLA-Restaurante");
//   ("Venezuela", "Lara", "LILA-Supermercado");
//   ("Venezuela", "Nueva Esparta", "LINO-Delicateses");
//   ("Venezuela", "Táchira", "HILARION-Abastos")]

let totalOrderQuantity =
    query { for order in db.OrderDetails do
            sumBy (int order.Quantity) }

let customersAverageOrders = 
    query { for c in db.Customers do 
            averageBy (float c.Orders.Count) }

//SELECT SUM(CONVERT(Int,[t0].[Quantity])) AS [value]
//FROM [dbo].[Order Details] AS [t0]
//-- Context: SqlProvider(Sql2008) Model: AttributedMetaModel Build: 4.0.30319.17626
//
//SELECT AVG([t2].[value]) AS [value]
//FROM (
//    SELECT CONVERT(Float,(
//        SELECT COUNT(*)
//        FROM [dbo].[Orders] AS [t1]
//        WHERE [t1].[CustomerID] = [t0].[CustomerID]
//        )) AS [value]
//    FROM [dbo].[Customers] AS [t0]
//    ) AS [t2]
//-- Context: SqlProvider(Sql2008) Model: AttributedMetaModel Build: 4.0.30319.17626

//val totalOrderQuantity : int = 51317
//val customersAverageOrders : float = 9.120879121

let averagePriceOverProductRange =
    query { for p in db.Products do
            averageByNullable p.UnitPrice }
//SELECT AVG([t0].[UnitPrice]) AS [value]
//FROM [dbo].[Products] AS [t0]
//-- Context: SqlProvider(Sql2008) Model: AttributedMetaModel Build: 4.0.30319.17626

//val averagePriceOverProductRange : System.Nullable<decimal> = 28.8663M

open System
let totalOrderQuantity =
    query { for c in db.Customers do 
            let numOrders = 
                query { for o in c.Orders do 
                        for od in o.OrderDetails do 
                        sumByNullable (Nullable(int od.Quantity)) }
            let averagePrice = 
                query { for o in c.Orders do 
                        for od in o.OrderDetails do 
                        averageByNullable (Nullable(od.UnitPrice)) }
            select (c.ContactName, numOrders, averagePrice) }
    |> Seq.toList
//SELECT [t0].[ContactName] AS [Item1], (
//    SELECT SUM([t3].[value])
//    FROM (
//        SELECT CONVERT(Int,[t2].[Quantity]) AS [value], [t1].[CustomerID], [t2].[OrderID], [t1].[OrderID] AS [OrderID2]
//        FROM [dbo].[Orders] AS [t1], [dbo].[Order Details] AS [t2]
//        ) AS [t3]
//    WHERE ([t3].[CustomerID] = [t0].[CustomerID]) AND ([t3].[OrderID] = [t3].[OrderID2])
//    ) AS [Item2], (
//    SELECT AVG([t6].[value])
//    FROM (
//        SELECT [t5].[UnitPrice] AS [value], [t4].[CustomerID], [t5].[OrderID], [t4].[OrderID] AS [OrderID2]
//        FROM [dbo].[Orders] AS [t4], [dbo].[Order Details] AS [t5]
//        ) AS [t6]
//    WHERE ([t6].[CustomerID] = [t0].[CustomerID]) AND ([t6].[OrderID] = [t6].[OrderID2])
//    ) AS [Item3]
//FROM [dbo].[Customers] AS [t0]
//-- Context: SqlProvider(Sql2008) Model: AttributedMetaModel Build: 4.0.30319.17626

//val totalOrderQuantity :
//  (string * System.Nullable<int> * System.Nullable<decimal>) list =
//  [("Maria Anders", 174, 26.7375M); ("Ana Trujillo", 63, 21.5050M);
//   ("Antonio Moreno", 359, 21.7194M); ("Thomas Hardy", 650, 19.1766M);
//   ...
//   ("Zbyszek Piestrzeniewicz", 205, 20.6312M)]

let productsGroupedByNameAndCountedTest1 =
    query { for p in db.Products do
            groupBy p.Category.CategoryName into group
            let sum = 
                query { for p in group do
                        sumBy (int p.UnitsInStock.Value) }
            select (group.Key, sum) }
    |> Seq.toList
//SELECT SUM(CONVERT(Int,[t0].[UnitsInStock])) AS [Item2], [t1].[CategoryName] AS [Item1]
//FROM [dbo].[Products] AS [t0]
//LEFT OUTER JOIN [dbo].[Categories] AS [t1] ON [t1].[CategoryID] = [t0].[CategoryID]
//GROUP BY [t1].[CategoryName]
//-- Context: SqlProvider(Sql2008) Model: AttributedMetaModel Build: 4.0.30319.17626

//val productsGroupedByNameAndCountedTest1 : (string * int) list =
//  [("Beverages", 559); ("Condiments", 507); ("Confections", 386);
//   ("Dairy Products", 393); ("Grains/Cereals", 308); ("Meat/Poultry", 165);
//   ("Produce", 100); ("Seafood", 701)]

let innerJoinQuery = 
    query { for c in db.Categories do
            join p in db.Products on (c.CategoryID =? p.CategoryID) 
            select (p.ProductName, c.CategoryName) } 
    |> Seq.toList
//SELECT [t1].[ProductName] AS [Item1], [t0].[CategoryName] AS [Item2]
//FROM [dbo].[Categories] AS [t0]
//INNER JOIN [dbo].[Products] AS [t1] ON ([t0].[CategoryID]) = [t1].[CategoryID]
//-- Context: SqlProvider(Sql2008) Model: AttributedMetaModel Build: 4.0.30319.17626

//val innerJoinQuery : (string * string) list =
//  [("Chai", "Beverages"); ("Chang", "Beverages");
//   ("Aniseed Syrup", "Condiments");
//   ...
//   ("Lakkalikööri", "Beverages");
//   ("Original Frankfurter grüne Soße", "Condiments")]

let innerJoinQuery = 
    query { for p in db.Products do
            select (p.ProductName, p.CategoryName) }
    |> Seq.toList
//SELECT [t0].[ProductName] AS [Item1], [t2].[test], [t2].[CategoryID], [t2].[CategoryName], [t2].[Description], [t2].[Picture]
//FROM [dbo].[Products] AS [t0]
//LEFT OUTER JOIN (
//    SELECT 1 AS [test], [t1].[CategoryID], [t1].[CategoryName], [t1].[Description], [t1].[Picture]
//    FROM [dbo].[Categories] AS [t1]
//    ) AS [t2] ON [t2].[CategoryID] = [t0].[CategoryID]
//-- Context: SqlProvider(Sql2008) Model: AttributedMetaModel Build: 4.0.30319.17626

//val innerJoinQuery : (string * NorthwndDb.ServiceTypes.Category) list =
//  [("Chai", Category); ("Chang", Category); ("Aniseed Syrup", Category);
//   ("Chef Anton's Cajun Seasoning", Category);
//   ...
//   ("Rhönbräu Klosterbier", Category); ("Lakkalikööri", Category);
//   ("Original Frankfurter grüne Soße", Category)]

let innerGroupJoinQueryWithAggregation =
    query { for c in db.Categories do
            groupJoin p in db.Products on (c.CategoryID =? p.CategoryID) into prodGroup
            let groupMax = query { for p in prodGroup do maxByNullable p.UnitsOnOrder }
            select (c.CategoryName, groupMax) }
    |> Seq.toList
//SELECT [t0].[CategoryName] AS [Item1], (
//    SELECT MAX([t1].[UnitsOnOrder])
//    FROM [dbo].[Products] AS [t1]
//    WHERE ([t0].[CategoryID]) = [t1].[CategoryID]
//    ) AS [Item2]
//FROM [dbo].[Categories] AS [t0]
//-- Context: SqlProvider(Sql2008) Model: AttributedMetaModel Build: 4.0.30319.17626

//val innerGroupJoinQueryWithAggregation : (string * Nullable<int16>) list =
//  [("Beverages", 40s); ("Condiments", 100s); ("Confections", 70s);
//   ("Dairy Products", 70s); ("Grains/Cereals", 80s); ("Meat/Poultry", 0s);
//   ("Produce", 20s); ("Seafood", 70s)]

open FSharp.Data

[<Literal>]
let connectionString = 
    @"Data Source=.;Initial Catalog=AdventureWorks2014;Integrated Security=True"

let cmd = 
    new SqlCommandProvider<
       "SELECT TOP(@topN) FirstName, LastName, SalesYTD 
        FROM Sales.vSalesPerson
        WHERE CountryRegionName = @regionName AND SalesYTD > @salesMoreThan 
        ORDER BY SalesYTD" , connectionString>()

cmd.Execute(topN = 3L, regionName = "United States", salesMoreThan = 1000000M) |> printfn "%A"

open System.Data
open System.Data.SqlClient

let connString = @"Server='.\SQLEXPRESS';Integrated Security=SSPI"
let conn = new SqlConnection(connString)
//val connString : string = "Server='.\SQLEXPRESS';Integrated Security=SSPI"
//val conn : SqlConnection = System.Data.SqlClient.SqlConnection

conn.Open()

open System.Data
open System.Data.SqlClient

let execNonQuery conn s =
    let comm = new SqlCommand(s, conn, CommandTimeout = 10)
    comm.ExecuteNonQuery() |> ignore
//val execNonQuery : conn:SqlConnection -> s:string -> unit

execNonQuery conn "CREATE DATABASE company"

execNonQuery conn "CREATE TABLE Employees (
   EmpID int NOT NULL,
   FirstName varchar(50) NOT NULL,
   LastName varchar(50) NOT NULL,
   Birthday datetime,
   PRIMARY KEY (EmpID))"

execNonQuery conn "INSERT INTO Employees (EmpId, FirstName, LastName, Birthday)
   VALUES (1001, 'Joe', 'Smith', '02/14/1965')"

execNonQuery conn "INSERT INTO Employees (EmpId, FirstName, LastName, Birthday)
   VALUES (1002, 'Mary', 'Jones', '09/15/1985')"

let query() =
    seq {
        use conn = new SqlConnection(connString)
        conn.Open()
        use comm = new SqlCommand("SELECT FirstName, Birthday FROM Employees", conn)
        use reader = comm.ExecuteReader()
        while reader.Read() do
            yield (reader.GetString 0, reader.GetDateTime 1)
    }
//val query : unit -> seq<string * System.DateTime>

fsi.AddPrinter(fun (d : System.DateTime) -> d.ToString())
query()
//val it : seq<string * System.DateTime> =
//  seq [("Joe", 14/02/1965 12:00:00 AM); ("Mary", 15/09/1985 12:00:00 AM)]

query() |> Seq.iter (fun (fn, bday) -> printfn "%s has birthday %O" fn bday)
//Joe has birthday 14/02/1965 00:00:00
//Mary has birthday 15/09/1985 00:00:00

query()
  |> Seq.filter (fun (nm, bday) -> bday < System.DateTime.Parse("01/01/1985"))
  |> Seq.length;;
//val it : int = 1

execNonQuery conn "
CREATE PROCEDURE dbo.GetEmployeesByLastName ( @Name nvarchar(50) ) AS
    SELECT Employees.FirstName, Employees.LastName
    FROM Employees
    WHERE Employees.LastName LIKE @Name"

let GetEmployeesByLastName (name : string) =
    use comm = new SqlCommand("GetEmployeesByLastName", conn,
                              CommandType = CommandType.StoredProcedure)

    comm.Parameters.AddWithValue("@Name", name) |> ignore
    use adapter = new SqlDataAdapter(comm)
    let table = new DataTable()
    adapter.Fill(table) |> ignore
    table
//val GetEmployeesByLastName : name:string -> DataTable

for row in GetEmployeesByLastName("Smith").Rows do
     printfn "row = %O, %O" (row.Item "FirstName") (row.Item "LastName")
//row = Joe, Smith