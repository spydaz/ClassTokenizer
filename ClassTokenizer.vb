''' <summary>
''' Tokenizer For NLP Techniques
''' </summary>
<ComClass(ClassTokenizer.ClassId, ClassTokenizer.InterfaceId, ClassTokenizer.EventsId)>
Public Class ClassTokenizer
    Public Const ClassId As String = "2899E490-7702-401C-BAB3-38FF97BC1AC9"
    Public Const EventsId As String = "CD994307-F53E-401A-AC6D-3CF8086FD6F1"
    Public Const InterfaceId As String = "8B9945F1-5D13-4059-829B-B531310144B5"
    'Tokenizer
    ''' <summary>
    ''' Returns Characters in String as list
    ''' </summary>
    ''' <param name="InputStr"></param>
    ''' <returns></returns>
    Public Function Tokenizer(ByRef InputStr As String) As List(Of String)
        Tokenizer = New List(Of String)
        InputStr = GetValidTokens(InputStr)

        Dim Endstr As Integer = InputStr.Length
        For i = 0 To Endstr
            Tokenizer.Add(InputStr(i))
        Next
    End Function
    ''' <summary>
    ''' Return Tokens in string divided by seperator
    ''' </summary>
    ''' <param name="InputStr"></param>
    ''' <param name="Divider">  </param>
    ''' <returns></returns>
    Public Function Tokenizer(ByRef InputStr As String, ByRef Divider As String) As List(Of String)
        Tokenizer = New List(Of String)
        InputStr = GetValidTokens(InputStr)
        Dim Tokens() As String = InputStr.Split(Divider)

        For Each item In Tokens
            Tokenizer.Add(item)
        Next
    End Function
    ''' <summary>
    ''' Gets Frequency of token
    ''' </summary>
    ''' <param name="Token"></param>
    ''' <param name="InputStr"></param>
    ''' <returns></returns>
    Public Function GetTokenFrequency(ByRef Token As String, ByRef InputStr As String) As Integer
        GetTokenFrequency = 0
        InputStr = GetValidTokens(InputStr)
        If InputStr.Contains(Token) = True Then
            For Each item In GetWordFrequecys(InputStr, " ")
                If item.word = Token Then
                    GetTokenFrequency = item.frequency
                End If
            Next
        End If
    End Function
    Private Function GetWordFrequecys(ByVal _Text As String, ByVal Delimiter As String) As List(Of WordFrequecys)
        Dim Words As New WordFrequecys
        Dim TempArray() As String = _Text.Split(Delimiter)
        Dim WordList As New List(Of String)
        Dim ListOfWordFrequecys As New List(Of WordFrequecys)
        For Each word As String In TempArray
            WordList.Add(word)
        Next
        Dim groups = WordList.GroupBy(Function(value) value)
        For Each grp In groups
            Words.word = grp(0)
            Words.frequency = grp.Count
            ListOfWordFrequecys.Add(Words)
        Next
        Return ListOfWordFrequecys
    End Function

    ''' <summary>
    ''' each charcter can be defined as a particular token enabling for removal of unwanted token types
    ''' </summary>
    ''' <param name="CharStr"></param>
    ''' <returns></returns>
    Public Function GetTokenType(ByRef CharStr As String) As TokenType
        For Each item In SeperatorPunctuation
            If CharStr = item Then Return TokenType.SeperatorPunctuation
        Next
        For Each item In GramaticalPunctuation
            If CharStr = item Then Return TokenType.GramaticalPunctuation
        Next
        For Each item In EncapuslationPunctuationStart
            If CharStr = item Then Return TokenType.EncapuslationPunctuationStart
        Next
        For Each item In EncapuslationPunctuationEnd
            If CharStr = item Then Return TokenType.EncapuslationPunctuationEnd
        Next
        For Each item In MoneyPunctuation
            If CharStr = item Then Return TokenType.MoneyPunctuation
        Next
        For Each item In MathPunctuation
            If CharStr = item Then Return TokenType.MathPunctuation
        Next
        For Each item In CodePunctuation
            If CharStr = item Then Return TokenType.CodePunctuation
        Next
        For Each item In AlphaBet
            If CharStr = item Then Return TokenType.AlphaBet
        Next
        For Each item In Number
            If CharStr = item Then Return TokenType.Number
        Next
        Return TokenType.Ignore
    End Function

    ''' <summary>
    ''' Returns valid tokens only tokens that are not defined are removed
    ''' </summary>
    ''' <param name="InputStr"></param>
    ''' <returns></returns>
    Public Function GetValidTokens(ByRef InputStr As String) As String
        Dim EndStr As Integer = InputStr.Length
        Dim CharStr As String = ""
        For i = 0 To EndStr - 1
            If GetTokenType(InputStr(i)) <> TokenType.Ignore Then
                CharStr = CharStr.AddSuffix(InputStr(i))
            Else

            End If
        Next
        Return CharStr
    End Function
    Public Function AlphanumericOnly(ByRef Str As String) As String
        Str = Str.GetValidTokens
        Str = RemoveTokenType(Str, TokenType.CodePunctuation)
        Str = RemoveTokenType(Str, TokenType.EncapuslationPunctuationEnd)
        Str = RemoveTokenType(Str, TokenType.EncapuslationPunctuationStart)
        Str = RemoveTokenType(Str, TokenType.MathPunctuation)
        Str = RemoveTokenType(Str, TokenType.MoneyPunctuation)
        Str = RemoveTokenType(Str, TokenType.GramaticalPunctuation)
        Str = Str.Remove(",")
        Str = Str.Remove("|")
        Str = Str.Remove("_")
        Return Str
    End Function
    ''' <summary>
    ''' Removes Tokens From String by Type
    ''' </summary>
    ''' <param name="UserStr"></param>
    ''' <param name="nType">  </param>
    ''' <returns></returns>
    Public Function RemoveTokenType(ByRef UserStr As String, ByRef nType As TokenType) As String

        Select Case nType
            Case TokenType.GramaticalPunctuation
                For Each item In GramaticalPunctuation
                    If UCase(UserStr).Contains(UCase(item)) = True Then
                        UserStr = UCase(UserStr).Remove(UCase(item))
                    End If
                Next
            Case TokenType.AlphaBet
                For Each item In AlphaBet
                    If UCase(UserStr).Contains(UCase(item)) = True Then
                        UserStr = UCase(UserStr).Remove(UCase(item))
                    End If
                Next
            Case TokenType.CodePunctuation
                For Each item In CodePunctuation
                    If UCase(UserStr).Contains(UCase(item)) = True Then
                        UserStr = UCase(UserStr).Remove(UCase(item))
                    End If
                Next
            Case TokenType.EncapuslationPunctuationEnd
                For Each item In EncapuslationPunctuationEnd
                    If UCase(UserStr).Contains(UCase(item)) = True Then
                        UserStr = UCase(UserStr).Remove(UCase(item))
                    End If
                Next
            Case TokenType.EncapuslationPunctuationStart
                For Each item In EncapuslationPunctuationStart
                    If UCase(UserStr).Contains(UCase(item)) = True Then
                        UserStr = UCase(UserStr).Remove(UCase(item))
                    End If
                Next
            Case TokenType.Ignore
            Case TokenType.MathPunctuation
                For Each item In MathPunctuation
                    If UCase(UserStr).Contains(UCase(item)) = True Then
                        UserStr = UCase(UserStr).Remove(UCase(item))
                    End If
                Next
            Case TokenType.MoneyPunctuation
                For Each item In MoneyPunctuation
                    If UCase(UserStr).Contains(UCase(item)) = True Then
                        UserStr = UCase(UserStr).Remove(UCase(item))
                    End If
                Next
            Case TokenType.Number
                For Each item In Number
                    If UCase(UserStr).Contains(UCase(item)) = True Then
                        UserStr = UCase(UserStr).Remove(UCase(item))
                    End If
                Next
            Case TokenType.SeperatorPunctuation
                For Each item In SeperatorPunctuation
                    If UCase(UserStr).Contains(UCase(item)) = True Then
                        UserStr = UCase(UserStr).Remove(UCase(item))
                    End If
                Next

        End Select
        Return UserStr
    End Function

    'Form Extensions
    ''' <summary>
    ''' Counts tokens in string
    ''' </summary>
    ''' <param name="InputStr"></param>
    ''' <param name="Delimiter"></param>
    ''' <returns></returns>
    Public Function CountTokens(ByRef InputStr As String, ByRef Delimiter As String) As Integer
        Dim Words() As String = Split(InputStr, Delimiter)
        Return Words.Count
    End Function
    ''' <summary>
    ''' Checks if input contains Ecapuslation Punctuation
    ''' </summary>
    ''' <param name="Userinput"></param>
    ''' <returns></returns>
    Public Function ContainsEncapsulated(ByRef Userinput As String) As Boolean
        Dim Start = False
        Dim Ending = False
        ContainsEncapsulated = False
        For Each item In EncapuslationPunctuationStart
            If Userinput.Contains(item) = True Then Start = True
        Next
        For Each item In EncapuslationPunctuationEnd
            If Userinput.Contains(item) = True Then Ending = True
        Next
        If Start And Ending = True Then
            ContainsEncapsulated = True
        End If
    End Function
    ''' <summary>
    ''' Gets encapsulated items found in userinput
    ''' </summary>
    ''' <param name="USerinput"></param>
    ''' <returns></returns>
    Public Function GetEncapsulated(ByRef Userinput As String) As List(Of String)
        GetEncapsulated = New List(Of String)
        Do Until ContainsEncapsulated(Userinput) = False
            GetEncapsulated.Add(ExtractEncapsulated(Userinput))
        Loop
    End Function
    ''' <summary>
    ''' Extracts first Encapsulated located in string
    ''' </summary>
    ''' <param name="Userinput"></param>
    ''' <returns></returns>
    Public Function ExtractEncapsulated(ByRef Userinput As String) As String
        ExtractEncapsulated = Userinput
        If ContainsEncapsulated(ExtractEncapsulated) = True Then
            If ExtractEncapsulated.Contains("(") = True And ExtractEncapsulated.Contains(")") = True Then
                ExtractEncapsulated = ExtractEncapsulated.ExtractStringBetween("(", ")")
            End If
            If Userinput.Contains("[") = True And Userinput.Contains("]") = True Then
                ExtractEncapsulated = ExtractEncapsulated.ExtractStringBetween("[", "]")
            End If
            If Userinput.Contains("{") = True And Userinput.Contains("}") = True Then
                ExtractEncapsulated = ExtractEncapsulated.ExtractStringBetween("{", "}")
            End If
            If Userinput.Contains("<") = True And Userinput.Contains(">") = True Then
                ExtractEncapsulated = ExtractEncapsulated.ExtractStringBetween("<", ">")
            End If
        End If
    End Function
End Class
''' <summary>
''' Grammer and syntax Components
''' </summary>
Public Module Constituants
    ''' <summary>
    ''' Recognized Tokens
    ''' </summary>
    Public Enum TokenType
        GramaticalPunctuation
        EncapuslationPunctuationStart
        EncapuslationPunctuationEnd
        MoneyPunctuation
        MathPunctuation
        CodePunctuation
        AlphaBet
        Number
        SeperatorPunctuation
        Ignore
    End Enum
    Public ReadOnly AlphaBet() As String = {"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N",
        "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",
        "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"}
    Public ReadOnly CodePunctuation() As String = {"\", "#", "@", "^"}
    Public ReadOnly EncapuslationPunctuationEnd() As String = {"}", "]", ">", ")"}
    Public ReadOnly EncapuslationPunctuationStart() As String = {"{", "[", "<", "("}
    Public ReadOnly GramaticalPunctuation() As String = {".", "?", "!", ":", ";"}
    Public ReadOnly MathPunctuation() As String = {"+", "-", "=", "/", "*", "%", "PLUS", "ADD", "MINUS", "SUBTRACT", "DIVIDE", "DIFFERENCE", "TIMES", "MULTIPLY", "PERCENT", "EQUALS"}
    Public ReadOnly MoneyPunctuation() As String = {"£", "$"}
    Public ReadOnly Number() As String = {"1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
    "30", "40", "50", "60", "70", "80", "90", "00", "000", "0000", "00000", "000000", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen",
    "nineteen", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety", "hundred", "thousand", "million", "Billion"}

    Public ReadOnly SeperatorPunctuation() = {" ", ",", "|"}
    Public Structure WordFrequecys
        Public Overrides Function ToString() As String
            Dim Str As String = ""
            Str = Str & "Word :" & word & vbCrLf
            Str = Str & "frequency :" & frequency & vbCrLf
            Return Str
        End Function

        Public frequency As Integer
        Public word As String
    End Structure

End Module
''' <summary>
''' Extension Methods for tokeninzer Fucntions
''' </summary>
Public Module TokenizerExt
    'Form Extensions
    ''' <summary>
    ''' Counts tokens in string
    ''' </summary>
    ''' <param name="InputStr"></param>
    ''' <param name="Delimiter"></param>
    ''' <returns></returns>
    <System.Runtime.CompilerServices.Extension>
    Public Function CountTokens(ByRef InputStr As String, ByRef Delimiter As String) As Integer
        Dim Words() As String = Split(InputStr, Delimiter)
        Return Words.Count
    End Function
    <Runtime.CompilerServices.Extension()>
    Public Function AlphanumericOnly(ByRef Str As String) As String
        Str = Str.GetValidTokens
        Str = RemoveTokenType(Str, TokenType.CodePunctuation)
        Str = RemoveTokenType(Str, TokenType.EncapuslationPunctuationEnd)
        Str = RemoveTokenType(Str, TokenType.EncapuslationPunctuationStart)
        Str = RemoveTokenType(Str, TokenType.MathPunctuation)
        Str = RemoveTokenType(Str, TokenType.MoneyPunctuation)
        Str = RemoveTokenType(Str, TokenType.GramaticalPunctuation)
        Str = Str.Remove(",")
        Str = Str.Remove("|")
        Str = Str.Remove("_")
        Return Str
    End Function
    ''' <summary>
    ''' Removes Tokens From String by Type
    ''' </summary>
    ''' <param name="UserStr"></param>
    ''' <param name="nType">  </param>
    ''' <returns></returns>
    <Runtime.CompilerServices.Extension()>
    Public Function RemoveTokenType(ByRef UserStr As String, ByRef nType As TokenType) As String

        Select Case nType
            Case TokenType.GramaticalPunctuation
                For Each item In GramaticalPunctuation
                    If UCase(UserStr).Contains(UCase(item)) = True Then
                        UserStr = UCase(UserStr).Remove(UCase(item))
                    End If
                Next
            Case TokenType.AlphaBet
                For Each item In AlphaBet
                    If UCase(UserStr).Contains(UCase(item)) = True Then
                        UserStr = UCase(UserStr).Remove(UCase(item))
                    End If
                Next
            Case TokenType.CodePunctuation
                For Each item In CodePunctuation
                    If UCase(UserStr).Contains(UCase(item)) = True Then
                        UserStr = UCase(UserStr).Remove(UCase(item))
                    End If
                Next
            Case TokenType.EncapuslationPunctuationEnd
                For Each item In EncapuslationPunctuationEnd
                    If UCase(UserStr).Contains(UCase(item)) = True Then
                        UserStr = UCase(UserStr).Remove(UCase(item))
                    End If
                Next
            Case TokenType.EncapuslationPunctuationStart
                For Each item In EncapuslationPunctuationStart
                    If UCase(UserStr).Contains(UCase(item)) = True Then
                        UserStr = UCase(UserStr).Remove(UCase(item))
                    End If
                Next
            Case TokenType.Ignore
            Case TokenType.MathPunctuation
                For Each item In MathPunctuation
                    If UCase(UserStr).Contains(UCase(item)) = True Then
                        UserStr = UCase(UserStr).Remove(UCase(item))
                    End If
                Next
            Case TokenType.MoneyPunctuation
                For Each item In MoneyPunctuation
                    If UCase(UserStr).Contains(UCase(item)) = True Then
                        UserStr = UCase(UserStr).Remove(UCase(item))
                    End If
                Next
            Case TokenType.Number
                For Each item In Number
                    If UCase(UserStr).Contains(UCase(item)) = True Then
                        UserStr = UCase(UserStr).Remove(UCase(item))
                    End If
                Next
            Case TokenType.SeperatorPunctuation
                For Each item In SeperatorPunctuation
                    If UCase(UserStr).Contains(UCase(item)) = True Then
                        UserStr = UCase(UserStr).Remove(UCase(item))
                    End If
                Next

        End Select
        Return UserStr
    End Function

    ''' <summary>
    ''' Returns Characters in String as list
    ''' </summary>
    ''' <param name="InputStr"></param>
    ''' <returns></returns>
    <Runtime.CompilerServices.Extension()>
    Public Function Tokenizer(ByRef InputStr As String) As List(Of String)
        Tokenizer = New List(Of String)
        InputStr = GetValidTokens(InputStr)

        Dim Endstr As Integer = InputStr.Length
        For i = 0 To Endstr
            Tokenizer.Add(InputStr(i))
        Next
    End Function
    ''' <summary>
    ''' Return Tokens in string divided by seperator
    ''' </summary>
    ''' <param name="InputStr"></param>
    ''' <param name="Divider">  </param>
    ''' <returns></returns>
    <Runtime.CompilerServices.Extension()>
    Public Function Tokenizer(ByRef InputStr As String, ByRef Divider As String) As List(Of String)
        Tokenizer = New List(Of String)
        InputStr = GetValidTokens(InputStr)
        Dim Tokens() As String = InputStr.Split(Divider)

        For Each item In Tokens
            Tokenizer.Add(item)
        Next
    End Function
    <Runtime.CompilerServices.Extension()>
    Public Function GetTokenFrequency(ByRef Token As String, ByRef InputStr As String) As Integer
        GetTokenFrequency = 0
        InputStr = GetValidTokens(InputStr)
        If InputStr.Contains(Token) = True Then
            For Each item In GetWordFrequecys(InputStr, " ")
                If item.word = Token Then
                    GetTokenFrequency = item.frequency
                End If
            Next
        End If
    End Function
    Private Function GetWordFrequecys(ByVal _Text As String, ByVal Delimiter As String) As List(Of WordFrequecys)
        Dim Words As New WordFrequecys
        Dim TempArray() As String = _Text.Split(Delimiter)
        Dim WordList As New List(Of String)
        Dim ListOfWordFrequecys As New List(Of WordFrequecys)
        For Each word As String In TempArray
            WordList.Add(word)
        Next
        Dim groups = WordList.GroupBy(Function(value) value)
        For Each grp In groups
            Words.word = grp(0)
            Words.frequency = grp.Count
            ListOfWordFrequecys.Add(Words)
        Next
        Return ListOfWordFrequecys
    End Function
    ''' <summary>
    ''' each charcter can be defined as a particular token enabling for removal of unwanted token types
    ''' </summary>
    ''' <param name="CharStr"></param>
    ''' <returns></returns>
    <Runtime.CompilerServices.Extension()>
    Public Function GetTokenType(ByRef CharStr As String) As TokenType
        For Each item In SeperatorPunctuation
            If CharStr = item Then Return TokenType.SeperatorPunctuation
        Next
        For Each item In GramaticalPunctuation
            If CharStr = item Then Return TokenType.GramaticalPunctuation
        Next
        For Each item In EncapuslationPunctuationStart
            If CharStr = item Then Return TokenType.EncapuslationPunctuationStart
        Next
        For Each item In EncapuslationPunctuationEnd
            If CharStr = item Then Return TokenType.EncapuslationPunctuationEnd
        Next
        For Each item In MoneyPunctuation
            If CharStr = item Then Return TokenType.MoneyPunctuation
        Next
        For Each item In MathPunctuation
            If CharStr = item Then Return TokenType.MathPunctuation
        Next
        For Each item In CodePunctuation
            If CharStr = item Then Return TokenType.CodePunctuation
        Next
        For Each item In AlphaBet
            If CharStr = item Then Return TokenType.AlphaBet
        Next
        For Each item In Number
            If CharStr = item Then Return TokenType.Number
        Next
        Return TokenType.Ignore
    End Function
    ''' <summary>
    ''' Returns valid tokens only tokens that are not defined are removed
    ''' </summary>
    ''' <param name="InputStr"></param>
    ''' <returns></returns>
    <Runtime.CompilerServices.Extension()>
    Public Function GetValidTokens(ByRef InputStr As String) As String
        Dim EndStr As Integer = InputStr.Length
        Dim CharStr As String = ""
        For i = 0 To EndStr - 1
            If GetTokenType(InputStr(i)) <> TokenType.Ignore Then
                CharStr = CharStr.AddSuffix(InputStr(i))
            Else

            End If
        Next
        Return CharStr
    End Function
End Module
''' <summary>
''' Extension methods for string functions
''' </summary>
Public Module StringExtensions
    ''' <summary>
    ''' extracts string between defined strings
    ''' </summary>
    ''' <param name="value">   base sgtring</param>
    ''' <param name="strStart">Start string</param>
    ''' <param name="strEnd">  End string</param>
    ''' <returns></returns>
    <System.Runtime.CompilerServices.Extension()>
    Public Function ExtractStringBetween(ByVal value As String, ByVal strStart As String, ByVal strEnd As String) As String
        If Not String.IsNullOrEmpty(value) Then
            Dim i As Integer = value.IndexOf(strStart)
            Dim j As Integer = value.IndexOf(strEnd)
            Return value.Substring(i, j - i)
        Else
            Return value
        End If
    End Function
    ''' <summary>
    ''' Adds string to front of string (no spaces)
    ''' </summary>
    ''' <param name="Str">   base string</param>
    ''' <param name="Prefix">Add before (no spaces)</param>
    ''' <returns></returns>
    <Runtime.CompilerServices.Extension()>
    Public Function AddPrefix(ByRef Str As String, ByVal Prefix As String) As String
        Return Prefix & Str
    End Function
    ''' <summary>
    ''' Adds Suffix to String (No Spaces)
    ''' </summary>
    ''' <param name="Str">   Base string</param>
    ''' <param name="Suffix">To be added After</param>
    ''' <returns></returns>
    <Runtime.CompilerServices.Extension()>
    Public Function AddSuffix(ByRef Str As String, ByVal Suffix As String) As String
        Return Str & Suffix
    End Function
    ''' <summary>
    ''' Returns The last word in String
    ''' NOTE: String ois converted to Array then the last element is extracted Count-1
    ''' </summary>
    ''' <param name="InputStr"></param>
    ''' <returns>String</returns>
    <System.Runtime.CompilerServices.Extension()>
    Public Function ExtractLastWord(ByRef InputStr As String) As String
        Dim TempArr() As String = Split(InputStr, " ")
        Dim Count As Integer = TempArr.Count - 1
        Return TempArr(Count)
    End Function
    <System.Runtime.CompilerServices.Extension()>
    Public Function ExtractSuffix(ByRef InputStr As String) As String
        Dim TempArr() As String = Split(InputStr, " ")
        Dim Count As Integer = TempArr.Count - 1
        Return TempArr(Count)
    End Function
    <System.Runtime.CompilerServices.Extension()>
    Public Function ExtractLastChar(ByRef InputStr As String) As String
        ExtractLastChar = Right(InputStr, 1)
    End Function
    <System.Runtime.CompilerServices.Extension()>
    Public Function ExtractFirstWord(ByRef Statement As String) As String
        Dim StrArr() As String = Split(Statement, " ")
        Return StrArr(0)
    End Function
    <System.Runtime.CompilerServices.Extension()>
    Public Function ExtractPrefix(ByRef Statement As String) As String
        Dim StrArr() As String = Split(Statement, " ")
        Return StrArr(0)
    End Function

End Module
''' <summary>
''' Term Document matrix functions and Extension methods
''' </summary>
Public Module TermDocumentMatrixExtensions
    ''' <summary>
    ''' Returns n-Grams
    ''' </summary>
    ''' <param name="Str">Input Str</param>
    ''' <param name="Ngrams">number of Grams</param>
    ''' <returns></returns>
    <Runtime.CompilerServices.Extension()>
    Public Function GetNgrms(ByRef Str As String, ByRef Ngrams As Integer) As List(Of String)
        Dim NgramArr() As String = Split(Str, " ")
        Dim Length As Integer = NgramArr.Count
        Dim lst As New List(Of String)
        Dim Str2 As String = ""
        For i = 0 To Length - Ngrams
            Str2 = ""
            Dim builder As New System.Text.StringBuilder()
            builder.Append(Str2)
            For j = 0 To Ngrams - 1
                builder.Append(NgramArr(i + j) & " ")
            Next
            Str2 = builder.ToString()
            lst.Add(Str2)
        Next
        Return lst
    End Function
    ''' <summary>
    ''' IDF(t) = log_e(Total number of documents / Number of documents with term t in it).
    ''' </summary>
    ''' <param name="Word">     </param>
    ''' <param name="Documents"></param>
    ''' <returns></returns>
    <System.Runtime.CompilerServices.Extension>
    Public Function InverseTermDocumentFrequency(ByRef Word As String, ByRef Documents As List(Of String)) As Integer
        InverseTermDocumentFrequency = 0
        Dim Freqs As New List(Of Integer)
        Dim Count As Integer = 0
        For Each item In Documents
            Freqs.Add(item.TermFrequency(Word))
            If item.Contains(Word) = True Then
                Count += 1
            End If
        Next
        InverseTermDocumentFrequency = Math.Log(Documents.Count / Count)
    End Function
    ''' <summary>
    ''' IDF: Inverse Document Frequency, which measures how important a term Is. While computing TF,
    '''      all terms are considered equally important. However it Is known that certain terms, such
    '''      As "is", "of", And "that", may appear a lot Of times but have little importance. Thus we
    '''      need To weigh down the frequent terms While scale up the rare ones, by computing the following:
    ''' IDF(t) = log_e(Total number of documents / Number of documents with term t in it).
    ''' </summary>
    ''' <param name="Userinput"></param>
    ''' <param name="Term">     </param>
    ''' <returns></returns>
    <System.Runtime.CompilerServices.Extension>
    Public Function InverseTermFrequency(ByRef Userinput As String, ByRef Term As String) As Double
        Return Math.Log(GetTokenFrequency(Term, Userinput) / Userinput.CountTokens(" "))
    End Function
    ''' <summary>
    ''' TF: Term Frequency, which measures how frequently a term occurs In a document. Since every
    '''     document Is different In length, it Is possible that a term would appear much more times
    '''     In Long documents than shorter ones. Thus, the term frequency Is often divided by the
    '''     document length (aka. the total number Of terms In the document) As a way Of normalization:
    ''' TF(t) = (Number of times term t appears in a document) / (Total number of terms in the document).
    ''' </summary>
    ''' <param name="InputStr"></param>
    ''' <param name="Term">     </param>
    ''' <returns></returns>
    <System.Runtime.CompilerServices.Extension>
    Public Function TermFrequency(ByRef InputStr As String, ByRef Term As String) As Double
        Return Term.GetTokenFrequency(InputStr) / InputStr.CountTokens(" ")
    End Function
End Module
