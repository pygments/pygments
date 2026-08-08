namespace Contoso.Sales.Document;

using Microsoft.Sales.Customer;
using System.Utilities;

/// <summary>
/// Posts sales documents and reports on the result.
/// </summary>
/// <remarks>Doc comments start with three slashes.</remarks>
codeunit 50100 "Sales Post Helper" implements "IPost Handler"
{
    Access = Internal;
    Permissions = tabledata Customer = rm;

    var
        Totals: array[10] of Decimal;
        PostedMsg: Label 'Document %1 was posted.', Comment = '%1 = document number';
        Handled: Boolean;

    trigger OnRun()
    begin
        PostAll();
    end;

    /* A block comment.
       It spans several lines and may contain // and 'quotes'. */
    [IntegrationEvent(false, false)]
    local procedure OnBeforePost(var SalesHeader: Record "Sales Header"; var IsHandled: Boolean)
    begin
    end;

    [Scope('OnPrem')]
    [Obsolete('Use PostAll instead.', '25.0')]
    procedure "Post All Documents"(): Integer
    var
        SalesHeader: Record "Sales Header";
        Count: Integer;
    begin
        // Attributes only start a line; the brackets below are indexing.
        Totals[1] := 0;
        Totals[2] := Totals[1] + 12.5;

        SalesHeader.SetRange("Document Type", SalesHeader."Document Type"::Order);
        SalesHeader.SetRange("Posting Date", 20240115D, 20241231D);
        if SalesHeader.FindSet() then
            repeat
                OnBeforePost(SalesHeader, Handled);
                if not Handled then begin
                    Count += 1;
                    Message(PostedMsg, SalesHeader."No.");
                end;
            until SalesHeader.Next() = 0;
        exit(Count);
    end;

    procedure Strings() Result: Text
    var
        Path: Text[250];
        Url: Text;
        Snippet: Text;
        Escaped: Text;
        Verbatim: Text;
    begin
        // No backslash escapes, so this string ends at the last quote.
        Path := 'C:\temp\';
        Url := 'https://example.com/a//b';
        Snippet := 'this /* is not */ a comment';
        Escaped := 'It''s only a doubled quote';
        Verbatim := @'@Customer*';
        exit(Path + Url + Snippet + Escaped + Verbatim);
    end;

    // Code converted from C/AL is upper case, and much of the base
    // application still looks like this.
    LOCAL PROCEDURE SumBalances(VAR Customer: Record Customer): Decimal
    VAR
        Total: Decimal;
    BEGIN
        IF Customer.FINDSET() THEN
            REPEAT
                Total := Total + Customer."Balance (LCY)";
            UNTIL Customer.NEXT() = 0;
        EXIT(Total);
    END;

    local procedure Classify(Value: Integer): Text
    begin
        case Value of
            1 .. 9:
                exit('small');
            10:
                exit('ten');
            else
                exit('large');
        end;
    end;

    local procedure Times()
    var
        Stamp: DateTime;
        Started: Time;
        Empty: Date;
    begin
        Stamp := CurrentDateTime();
        Started := 120000T;
        Empty := 0D;
        if Stamp = 0DT then
            Error('No timestamp');
    end;
}

table 50100 "Sales Post Log"
{
    Caption = 'Sales Post Log';
    DataClassification = CustomerContent;

    fields
    {
        field(1; "Entry No."; Integer)
        {
            Caption = 'Entry No.';
            AutoIncrement = true;
        }
        field(2; "Document No."; Code[20])
        {
            Caption = 'Document No.';
            NotBlank = true;
        }
        field(3; Amount; Decimal)
        {
            CalcFormula = sum("Sales Line".Amount where("Document No." = field("Document No.")));
            FieldClass = FlowField;
        }
    }

    keys
    {
        key(PK; "Entry No.")
        {
            Clustered = true;
        }
        key(Doc; "Document No.")
        {
        }
    }

    fieldgroups
    {
        fieldgroup(DropDown; "Document No.", Amount)
        {
        }
    }
}

pageextension 50100 "Customer Card Ext" extends "Customer Card"
{
    layout
    {
        addlast(General)
        {
            field(PostCount; Rec."Post Count")
            {
                ApplicationArea = All;
                ToolTip = 'Specifies how often the customer was posted.';
            }
        }
        modify("Name 2")
        {
            Visible = false;
        }
    }

    actions
    {
        addafter(Approve)
        {
            action(PostAll)
            {
                ApplicationArea = All;
                Caption = 'Post All';
                Image = Post;

                trigger OnAction()
                var
                    Helper: Codeunit "Sales Post Helper";
                begin
                    Helper."Post All Documents"();
                end;
            }
        }
    }
}

enum 50100 "Post Result" implements "IPost Handler"
{
    Extensible = true;

    value(0; " ")
    {
        Caption = ' ';
    }
    value(1; Posted)
    {
        Caption = 'Posted';
        Implementation = "IPost Handler" = "Sales Post Helper";
    }
}

interface "IPost Handler"
{
    procedure Post(var SalesHeader: Record "Sales Header"): Boolean;
}

#pragma warning disable AL0432
#if not CLEAN25
codeunit 50101 "Obsolete Bridge"
{
    ObsoleteState = Pending;
    ObsoleteReason = 'Replaced by Sales Post Helper.';
}
#else
codeunit 50101 "Obsolete Bridge"
{
    ObsoleteState = Removed;
}
#endif
#pragma warning restore AL0432

#region DotNet aliases
dotnet
{
    assembly("System.Runtime")
    {
        type("System.DateTime"; NetDateTime)
        {
        }
    }
}
#endregion
