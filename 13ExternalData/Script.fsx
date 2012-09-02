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
//<wb:countries page="1"+[25149 chars]

> worldBankCountriesXmlPage1;;
//val it : string =
//  "<?xml version="1.0" encoding="utf-8"?>
//<wb:countries page="1" pages="5" per_page="50" total="246" xmlns:wb="http://www.worldbank.org">
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
//  </wb:country>
//</wb:countries>"

let worldBankCountriesJsonPage1 = http "http://api.worldbank.org/country?format=json"
//val worldBankCountriesJsonPage1 : string =
//  "[{"page":1,"pages":5,"per_page":"50","total":246},[{"id":"ABW"+[17322 chars]

#r "System.Xml.Linq.dll"
open System.Xml.Linq

/// The schema tag for this particular blob of XML
let xmlSchemaUrl = "http://www.worldbank.org"

// General utilities for XML
let xattr s (el : XElement) = el.Attribute(XName.Get(s)).Value
let xelem s (el : XContainer) = el.Element(XName.Get(s, xmlSchemaUrl))
let xelems s (el : XContainer) = el.Elements(XName.Get(s, xmlSchemaUrl))
let xvalue (el : XElement) = el.Value

/// The results of parsing
type Country = {Id : string; Name : string; Region : string}

/// Parse a page of countries from the WorldBank data source
let parseCountries xml = 
    let doc = XDocument.Parse xml
    [for countryXml in doc |> xelem "countries" |> xelems "country" do
        let region = countryXml |> xelem "region" |> xvalue
        yield
            {
                Id = countryXml |> xattr "id"
                Name = countryXml |> xelem "name" |> xvalue
                Region = region
            }]
//val xmlSchemaUrl : string = "http://www.worldbank.org"
//val xattr : s:string -> el:XElement -> string
//val xelem : s:string -> el:XContainer -> XElement
//val xelems :
//  s:string ->
//    el:XContainer -> System.Collections.Generic.IEnumerable<XElement>
//val xvalue : el:XElement -> string
//type Country =
//  {Id: string;
//   Name: string;
//   Region: string;}
//val parseCountries : xml:string -> Country list

> parseCountries worldBankCountriesXmlPage1;;
//val it : Country list =
//  [{Id = "ABW";
//    Name = "Aruba";
//    Region = "Latin America & Caribbean (all income levels)";};
//   {Id = "CYM";
//    Name = "Cayman Islands";
//    Region = "Latin America & Caribbean (all income levels)";}]

let worldBankCountriesXmlPage2 = http "http://api.worldbank.org/country?page=2"
//val worldBankCountriesXmlPage2 : string =
//  "<?xml version="1.0" encoding="utf-8"?>
//<wb:countries page="2"+[24286 chars]

let doc = XDocument.Parse worldBankCountriesXmlPage1
//val doc : XDocument =
//  <wb:countries page="1" pages="5" per_page="50" total="246" xmlns:wb="http://www.worldbank.org">
//  <wb:country id="ABW">
//  ...
//</wb:countries>

let totalPageCount = doc |> xelem "countries" |> xattr "pages" |> int
//val totalPageCount : int = 5

let rec getCountryPages() = 
    let page1 = http "http://api.worldbank.org/country"
    let doc1 = XDocument.Parse page1
    let numPages = doc1 |> xelem "countries" |> xattr "pages" |> int
    let otherPages = [for i in 2 .. numPages -> http ("http://api.worldbank.org/country?page=" + string i)]
    [
        yield! parseCountries page1
        for otherPage in otherPages do 
            yield! parseCountries otherPage
    ]
//val getCountryPages : unit -> Country list

> getCountryPages() |> Seq.length;;
//val it : int = 246

> http "http://api.worldbank.org/country?format=json&per_page=1000";;
//val it : string =
//  "[{"page":1,"pages":1,"per_page":"1000","total":246},...]]"

#r "System.Data.Services.Client.dll"
#r "FSharp.Data.TypeProviders.dll"
open Microsoft.FSharp.Data.TypeProviders

type Northwind = ODataService<"http://services.odata.org/Northwind/Northwind.svc/">
let db = Northwind.GetDataContext()

let first10Customers =
    query {for c in db.Customers do take 10; select c}
    |> Seq.toList
//type Northwind =
//  class
//    static member GetDataContext : unit -> Northwind.ServiceTypes.SimpleDataContextTypes.NorthwindEntities
//     + 1 overload
//    nested type ServiceTypes
//  end
//val db : Northwind.ServiceTypes.SimpleDataContextTypes.NorthwindEntities
//val first10Customers : Northwind.ServiceTypes.Customer list =
//  [Customer; Customer; Customer; Customer; Customer; Customer; Customer;
//   Customer; Customer; Customer]

> first10Customers;;
//val it : Northwind.ServiceTypes.Customer list =
//  [Customer {Address = "Obere Str. 57";
//             City = "Berlin";
//             CompanyName = "Alfreds Futterkiste";
//             ContactName = "Maria Anders";
//             ContactTitle = "Sales Representative";
//             Country = "Germany";
//             CustomerDemographics = seq [];
//             CustomerID = "ALFKI";
//             Fax = "030-0076545";
//             Orders = seq [];
//             Phone = "030-0074321";
//             PostalCode = "12209";
//             Region = null;};...]

db.DataContext.SendingRequest.Add (fun x -> printfn "requesting %A" x.Request.RequestUri)
//val it : unit = ()

query {for c in db.Customers do take 10; select c}
//requests http://services.odata.org/Northwind/Northwind.svc/Customers()?$top=10

query {for c in db.Customers do select c}
//requests http://services.odata.org/Northwind/Northwind.svc/Customers()

query {for c in db.Customers do where (c.ContactName.Contains "Maria"); take 10; select c}
//requesting http://services.odata.org/Northwind/Northwind.svc/Customers()?$filter=substringof('Maria',ContactName)&$top=10

query {for c in db.Customers.Expand("Orders") do select c}
//requesting http://services.odata.org/Northwind/Northwind.svc/Customers()?$expand=Orders

//db.DataContext.SendingRequest.Add(fun e -> 
//    e.RequestHeaders.["Authorization"] <- "Basic " + <base64 encoding of "username:password">)

//db.DataContext.SendingRequest.Add(fun e -> 
//    e.RequestHeaders.["Authorization"] <- "OAuth " + <securityToken>)

open System.Data.Services.Client
open System.Linq

let executePaginated (ctxt : DataServiceContext) (query : IQueryable<'T>) = 
    match query with 
    | :? DataServiceQuery<'T> as q ->
        seq { 
            let rec loop (cont : DataServiceQueryContinuation<'T>) = seq {
                if cont <> null then
                    let rsp = ctxt.Execute cont 
                    yield! rsp
                    yield! loop (rsp.GetContinuation())}
            let rsp = q.Execute() 
            yield! rsp
            let cont = (rsp :?> QueryOperationResponse<'T>).GetContinuation()
            yield! loop cont }
    | _ -> query.AsEnumerable()
//val executePaginated :
//  ctxt:DataServiceContext -> query:System.Linq.IQueryable<'T> -> seq<'T>

let allCustomersQuery = 
    query {for c in db.Customers do select c}
    |> executePaginated db.DataContext
    |> Seq.toList
//val allCustomersQuery : Northwind.ServiceTypes.Customer list =
//  [Customer; Customer; Customer; Customer; Customer; Customer; Customer;...]

#r "System.Data.Linq.dll"
#r "FSharp.Data.TypeProviders.dll"

open Microsoft.FSharp.Linq
open Microsoft.FSharp.Data.TypeProviders

[<Literal>]
let connectionString = @"Data Source=.;Initial Catalog=northwind;Integrated Security=SSPI;"
type NorthwndDb = 
    SqlDataConnection<ConnectionString = connectionString, Pluralize = true>
//type NorthwndDb =
//  class
//    static member GetDataContext : unit -> NorthwndDb.ServiceTypes.SimpleDataContextTypes.Northwind
//     + 1 overload
//    nested type ServiceTypes
//  end

let db = NorthwndDb.GetDataContext()

let customersSortedByCountry =
    query {for c in db.Customers do sortBy c.Country; select (c.Country, c.CompanyName)}
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
    query {
        for emp in db.Employees do
        where (emp.BirthDate.Value.Year > 1960)
        where (emp.LastName.StartsWith "S")
        select (emp.FirstName, emp.LastName) 
        take 5}
    |> Seq.toList
//SELECT TOP (5) [t0].[FirstName] AS [Item1], [t0].[LastName] AS [Item2]
//FROM [dbo].[Employees] AS [t0]
//WHERE ([t0].[LastName] LIKE @p0) AND (DATEPART(Year, [t0].[BirthDate]) > @p1)
//-- @p0: Input NVarChar (Size = 4000; Prec = 0; Scale = 0) [S%]
//-- @p1: Input Int (Size = -1; Prec = 0; Scale = 0) [1960]
//-- Context: SqlProvider(Sql2008) Model: AttributedMetaModel Build: 4.0.30319.17626

//val selectedEmployees : (string * string) list = [("Michael", "Suyama")]

let customersSortedTwoColumns =
    query {
        for c in db.Customers do 
        sortBy c.Country
        thenBy c.Region
        select (c.Country, c.Region, c.CompanyName)}
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

let totalOrderQuantity = query {for order in db.OrderDetails do sumBy (int order.Quantity)}
let customersAverageOrders = query {for c in db.Customers do averageBy (float c.Orders.Count)}
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

let averagePriceOverProductRange = query {for p in db.Products do averageByNullable p.UnitPrice}
//SELECT AVG([t0].[UnitPrice]) AS [value]
//FROM [dbo].[Products] AS [t0]
//-- Context: SqlProvider(Sql2008) Model: AttributedMetaModel Build: 4.0.30319.17626

//val averagePriceOverProductRange : System.Nullable<decimal> = 28.8663M

open System
let totalOrderQuantity =
    query {
        for c in db.Customers do 
            let numOrders = query {
                for o in c.Orders do 
                for od in o.OrderDetails do 
                sumByNullable (Nullable(int od.Quantity))}
            let averagePrice = query {
                for o in c.Orders do 
                for od in o.OrderDetails do 
                averageByNullable (Nullable(od.UnitPrice))}
            select (c.ContactName, numOrders, averagePrice)}
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
    query {
        for p in db.Products do
        groupBy p.Category.CategoryName into group
        let sum = query {for p in group do sumBy (int p.UnitsInStock.Value)}
        select (group.Key, sum)}
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
    query {
        for c in db.Categories do
        join p in db.Products on (c.CategoryID =? p.CategoryID) 
        select (p.ProductName, c.CategoryName)} //produces flat sequence
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
    query {
        for p in db.Products do
        select (p.ProductName, p.Category)}
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
    query {
        for c in db.Categories do
        groupJoin p in db.Products on (c.CategoryID =? p.CategoryID) into prodGroup
        let groupMax = query {for p in prodGroup do maxByNullable p.UnitsOnOrder}
        select (c.CategoryName, groupMax)}
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

open System.Data
open System.Data.SqlClient

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let connString = @"Server='.\SQLEXPRESS';Integrated Security=SSPI"
let conn = new SqlConnection(connString)
//val connString : string = "Server='.\SQLEXPRESS';Integrated Security=SSPI"
//val conn : SqlConnection = System.Data.SqlClient.SqlConnection

> conn.Open();;

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

let query() = seq {
    use conn = new SqlConnection(connString)
    conn.Open()
    use comm = new SqlCommand("SELECT FirstName, Birthday FROM Employees", conn)
    use reader = comm.ExecuteReader()
    while reader.Read() do
        yield (reader.GetString 0, reader.GetDateTime 1)}
//val query : unit -> seq<string * System.DateTime>

> fsi.AddPrinter(fun (d : System.DateTime) -> d.ToString());;
> query();;
//val it : seq<string * System.DateTime> =
//  seq [("Joe", 14/02/1965 12:00:00 AM); ("Mary", 15/09/1985 12:00:00 AM)]

> query() |> Seq.iter (fun (fn, bday) -> printfn "%s has birthday %O" fn bday);;
//Joe has birthday 14/02/1965 00:00:00
//Mary has birthday 15/09/1985 00:00:00

> query()
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

> for row in GetEmployeesByLastName("Smith").Rows do
     printfn "row = %O, %O" (row.Item "FirstName") (row.Item "LastName");;
//row = Joe, Smith

#r "System.ServiceModel.dll"
#r "FSharp.Data.TypeProviders.dll"

open Microsoft.FSharp.Data.TypeProviders

type Weather = WsdlService<"http://www.webservicex.com/globalweather.asmx?wsdl">
//type Weather =
//  class
//    static member GetGlobalWeatherSoap : unit -> Weather.ServiceTypes.SimpleDataContextTypes.GlobalWeatherSoapClient
//     + 1 overload
//    static member GetGlobalWeatherSoap12 : unit -> Weather.ServiceTypes.SimpleDataContextTypes.GlobalWeatherSoapClient
//     + 1 overload
//    nested type ServiceTypes
//  end

let ws = Weather.GetGlobalWeatherSoap();;
//val ws : Weather.ServiceTypes.SimpleDataContextTypes.GlobalWeatherSoapClient

let weatherInCanberra = ws.GetWeather("Canberra", "Australia")
//val weatherInCanberra : string =
//  "<?xml version="1.0" encoding="utf-16"?>
//<CurrentWeather>
//  <L"+[461 chars]

weatherInCanberra;;
//  "<?xml version="1.0" encoding="utf-16"?>
//<CurrentWeather>
//  <Location>Canberra, Australia (YSCB) 35-18S 149-11E 580M</Location>
//  <Time>Aug 14, 2012 - 12:00 PM EDT / 2012.08.14 1600 UTC</Time>
//  <Wind> from the E (080 degrees) at 7 MPH (6 KT):0</Wind>
//  <Visibility> greater than 7 mile(s):0</Visibility>
//  <SkyConditions> partly cloudy</SkyConditions>
//  <Temperature> 35 F (2 C)</Temperature>
//  <Wind>Windchill: 28 F (-2 C):1</Wind>
//  <DewPoint> 33 F (1 C)</DewPoint>
//  <RelativeHumidity> 93%</RelativeHumidity>
//  <Pressure> 30.00 in. Hg (1016 hPa)</Pressure>
//  <Status>Success</Status>
//</CurrentWeather>"