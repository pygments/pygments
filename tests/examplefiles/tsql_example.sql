-- Example Transact-SQL file.

-- Single line comment
/* A comment 
 * spawning two lines. */
                         /* An indented comment
                          * spawning multiple
                          * lines. */
/* A /* nested */ comment. */

select
    left(emp.firstname, 1) + '.' + [emp.surname] as "Name",
    dep.name as [Department]
into
    #temp_employee
from
    employee as emp
    inner join department as dep on
       dep.ident_code = emp.department_id
where
    emp.date_of_birth >= '1990-01-01';
go

declare @TextToFind nvarchar(100) = N'some
text across
multiple lines';

set @TextToFind varchar(32) = 'hello' + ' world';
set @TextTiFind += '!';

declare @Count int = 17 * (3 - 5);

delete from
    [server].[database].[schema].[table]
where
    [Text] = @TextToFind and author Not LIKE '%some%';

goto overthere;
overthere:

select
    123 as "int 1",
    +123 as "int 2",
    -123 as "int 3",
    0x20 as "hex int",
    123.45 as "float 1",
    -1.23e45 as "float 2"
    +1.23E+45 as "float 3",
    -1.23e-45 as "float 4",
    1. as "float 5",
    .1 as "float 6",
    1.e2 as "float 7",
    .1e2 as "float 8";

Select @@Error, $PARTITion.RangePF1(10);

select top 3 Ähnliches from Müll;

-- Example transaction
BEGIN TRAN

BEGIN TRY
   INSERT INTO #temp_employe(Name, Department) VALUES ('L. Miller', 'Sales')
   iNsErT inTO #temp_employe(Name, Department) VaLuEs ('M. Webster', 'Helpdesk')
   COMMIT TRAN
END TRY
BEGIN CATCH
   print 'cannot perform transaction; rolling back';
   ROLLBACK TRAN
END CATCH

-- Comment at end without newline.