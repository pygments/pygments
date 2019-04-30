rem VBScript examples

' Various constants of different types
const someText = "some " & """text"""
const someInt = 123
const someHex = &h3110c0d3
const someFloat = 123.45e-67
const someDate = #1/2/2016#
const someTime = #12:34:56 AM#
const someBool = vbTrue  ' -1

' Do some math.
radius = 1.e2
area = radius ^ 2 * 3.1315
a = 17 : b = 23
c = sqr(a ^2 + b ^ 2)

' Write 10 files.
For i = 1 to 10
    createFile( i )
Next

Public Sub createFile(a)
    Dim fso, TargetFile
    TargetPath = "C:\some_" & a & ".tmp"
    Set fso = CreateObject("Scripting.FileSystemObject")
    Set TargetFile = fso.CreateTextFile(TargetPath)
    TargetFile.WriteLine("Hello " & vbCrLf & "world!")
    TargetFile.Close
End Sub

' Define a class with a property.
Class Customer
    Private m_CustomerName

    Private Sub Class_Initialize
        m_CustomerName = ""
    End Sub

    ' CustomerName property.
    Public Property Get CustomerName
        CustomerName = m_CustomerName
    End Property

    Public Property Let CustomerName(custname)
        m_CustomerName = custname
    End Property
End Class

' Special constructs
Option Explicit
On Error Resume Next
On Error Goto 0

' Comment without terminating CR/LF.