Imports System.Collections.Generic

Public Enum Tile
    ANTS = 0
    DEAD = -1
    LAND = -2
    FOOD = -3
    WATER = -4
    UNSEEN = -5
End Enum

Public Enum Direction
    n
    s
    e
    w
End Enum

Public Structure Location
    Public Row As Long
    Public Col As Long
    Public Sub New(ByVal row As Long, ByVal col As Long)
        Me.Row = row
        Me.Col = col
    End Sub
End Structure

Public Class Ants
    ' Game Class Settings
    Public Rows As Long
    Public Cols As Long
    Public Turns As Long

    Public ViewRadius2 As Long
    Public AttackRadius2 As Long
    Public SpawnRadius2 As Long

    Public LoadTime As Long
    Public TurnTime As Long

    Public PlayerSeed As Long

    ' Properties updated during the game
    Public Turn As Long
    Public Map(,) As Tile
    Public HillList As Dictionary(Of Location, Long) = New Dictionary(Of Location, Long)
    Public AntList As Dictionary(Of Location, Long) = New Dictionary(Of Location, Long)
    Public FoodList As List(Of Location) = New List(Of Location)
    Public DeadList As Dictionary(Of Location, List(Of Long)) = New Dictionary(Of Location, List(Of Long))

    Private TurnStartTime As DateTime

    Public Sub ParseSetup(ByVal data As List(Of String))
        Dim line as String
        For Each line In data
            Dim token As String() = line.Split()
            Dim value As Long
            Try
                value = CLng(token(1))
            Catch ex As InvalidCastException
                Console.Error.WriteLine("Token " & token(0) & " not followed by an integer: " & token(1))
            Catch ex As Exception
                Console.Error.WriteLine("Failed to parse: " & token(0))
            End Try
            Select Case token(0)
                Case "rows"
                    Me.Rows = value
                Case "cols"
                    Me.Cols = value
                Case "turns"
                    Me.Turns = value
                Case "viewradius2"
                    Me.ViewRadius2 = value
                Case "attackradius2"
                    Me.AttackRadius2 = value
                Case "spawnradius2"
                    Me.SpawnRadius2 = value
                Case "loadtime"
                    Me.LoadTime = value
                Case "turntime"
                    Me.TurnTime = value
                Case "player_seed"
                    Me.PlayerSeed = value
            End Select
        Next
        ReDim Me.Map(Me.Rows - 1, Me.Cols - 1)
    End Sub

    Public Sub Update(ByVal data As List(Of String))
        Me.TurnStartTime = DateTime.Now

        ' Clear out lists
        For i = 0 To Me.Rows - 1
            For j = 0 To Me.Cols - 1
                If Me.Map(i, j) <> Tile.WATER Then
                    Me.Map(i, j) = Tile.LAND
                End If
            Next j
        Next i
        Me.HillList.Clear()
        Me.AntList.Clear()
        Me.FoodList.Clear()
        Me.DeadList.Clear()

        ' Parse input
        Dim line As String
        For Each line In data
            Dim token As String() = line.Split()
            Try
                Dim row As Long = CLng(token(1))
                Dim col As Long = CLng(token(2))
                Dim objLoc As Location = New Location(row, col)
                If token(0) = "w" Then
                    Me.Map(row, col) = Tile.WATER
                ElseIf token(0) = "f" Then
                    Me.Map(row, col) = Tile.FOOD
                    Me.FoodList.Add(objLoc)
                Else
                    Dim owner As Long = CLng(token(3))
                    If token(0) = "a" Then
                        Me.Map(row, col) = owner
                        Me.AntList.Add(objLoc, owner)
                    ElseIf token(0) = "d" Then
                        If Me.Map(row, col) = Tile.LAND Then
                            Me.Map(row, col) = Tile.DEAD
                        End If
                        If Not Me.DeadList.ContainsKey(objLoc) Then
                            Me.DeadList.Add(objLoc, New List(Of Long))
                        End If
                        Me.DeadList(objLoc).Add(owner)
                    ElseIf token(0) = "h" Then
                        Me.HillList(objLoc) = owner
                    End If
                End If
            Catch ex As Exception
                Console.Error.WriteLine("Error parsing line: " & line)
            End Try
        Next
    End Sub

    Public Function TimeRemaining() As Long
        Return 1000 - DateTime.Now.Subtract(Me.TurnStartTime).TotalMilliseconds
    End Function

    Public Sub IssueOrder(ByVal loc As Location, ByVal dir As Direction)
        Console.WriteLine("o " & loc.Row & " " & loc.Col & " " & dir.ToString)
    End Sub

    Public Sub FinishTurn()
        Console.WriteLine("go")
    End Sub

    Public Function MyHills() As List(Of Location)
        Dim hills As List(Of Location) = New List(Of Location)
        Dim hill As Location
        For Each hill In Me.HillList
            If hill.Value = 0 Then
                hills.Add(hill.Key)
            End If
        Next
        Return hills
    End Function

    Public Function EnemyHills() As List(Of Location)
        Dim hills As List(Of Location) = New List(Of Location)
        Dim hill As Location
        For Each hill In Me.HillList
            If hill.Value <> 0 Then
                hills.Add(hill.Key)
            End If
        Next
        Return hills
    End Function

    Public Function MyAnts() As List(Of Location)
        Dim ants As List(Of Location) = New List(Of Location)
        Dim ant As Location
        For Each ant In Me.AntList
            If ant.Value = 0 Then
                ants.Add(ant.Key)
            End If
        Next
        Return ants
    End Function

    Public Function EnemyAnts() As List(Of Location)
        Dim ants As List(Of Location) = New List(Of Location)
        Dim ant As Location
        For Each ant In Me.AntList
            If ant.Value <> 0 Then
                ants.Add(ant.Key)
            End If
        Next
        Return ants
    End Function

    Public Function Food() As List(Of Location)
        Dim foods = New List(Of Location)
        foods.AddRange(Me.FoodList)
        Return foods
    End Function

    Public Function Unoccupied(ByVal loc As Location) As Boolean
        Return Me.Map(loc.Row, loc.Col) = Tile.LAND OrElse Me.Map(loc.Row, loc.Col) = Tile.DEAD
    End Function

    Public Function Passable(ByVal loc As Location) As Boolean
        Return Me.Map(loc.Row, loc.Col) <> Tile.WATER
    End Function

    Public Function Aim(ByVal loc As Location, ByVal dir As Direction) As Location
        Dim dRow As Long = 0
        Dim dCol As Long = 0
        Select Case dir
            Case Direction.n
                dRow = -1
            Case Direction.s
                dRow = 1
            Case Direction.w
                dCol = -1
            Case Direction.e
                dCol = 1
        End Select
        Return New Location(dRow, dCol)
    End Function

    Public Function Destination(ByVal loc As Location, ByVal dir As Direction)
        Dim dloc As Location = Me.Aim(loc, dir)
        Dim dRow As Long = (loc.Row + dloc.Row) Mod Me.Rows
        If dRow < 0 Then
            dRow += Me.Rows
        End If
        Dim dCol As Long = (loc.Col + dloc.Col) Mod Me.Cols
        If dCol < 0 Then
            dCol += Me.Cols
        End If
        Return New Location(dRow, dCol)
    End Function

    Public Shared Sub Run(ByVal b As Bot)
        Dim a As New Ants()
        Dim mapData As List(Of String) = New List(Of String)
        Do While True
            Try
                Dim line As String = Console.ReadLine()
                If line Is Nothing Then
                    Exit Sub
                ElseIf line.ToLower = "ready" Then
                    a.ParseSetup(mapData)
                    b.DoSetup(a)
                    a.FinishTurn()
                    mapData = New List(Of String)
                ElseIf line.ToLower() = "go" Then
                    a.Update(mapData)
                    b.DoTurn(a)
                    a.FinishTurn()
                    mapData = New List(Of String)
                Else
                    mapData.Add(line)
                End If
            Catch ex As Exception
                Console.Error.WriteLine(ex.ToString)
                Exit Do
            End Try
        Loop
    End Sub

End Class


