package coffee.project;

import coffee.IdentifierList;
import coffee.TokenList;
import coffee.datatypes.*;

import java.util.*;

/**
 * Created by ftektas on 12/12/16.
 * Implemented by Mehmed MUSTAFA 131044084
 */
public class Parser {
    ArrayList<String> coffeeRules = new ArrayList<String>();
    ArrayList<String> StringOfTokens = new ArrayList<>();
    ArrayList<ArrayList<String>> outputStrings = new ArrayList<>();

    // Parses the lexer result and prints *ONLY* the parsing result.
    public void parse() {
        IdentifierList identifierList = IdentifierList.getInstance();
        TokenList tokenList = TokenList.getInstance();

        // Initialize all coffee rules
        AddCoffeeRules();
        // Get the string containing all the tokens
        StringOfTokens = produceTokenStrings(tokenList);
        //System.out.println(StringOfTokens.toString());

        // If there are mismatching parentheses
        if(!validateParentheses(StringOfTokens)){
            throw new IllegalArgumentException("\nERROR: There are mismatching parentheses!!!");
        }

        // At the beggining expression in the output
        outputStrings.add(StringOfTokens);

        try{
            // While there are tokens reduce one time
            // x - hata durumunda sonsuza dusmeyi engelliyor, test amacli koyuldu
            int x = 0;
            while(StringOfTokens != null && x<50){
                StringOfTokens = reduceOneTime(StringOfTokens, false);
                if(StringOfTokens == null)
                    break;

                // Save the new expressions in the output
                outputStrings.add(StringOfTokens);
                ++x;
            }
        } catch(IllegalArgumentException error){
            System.err.println(error.toString());
        }

        //Reverse the output
        Collections.reverse(outputStrings);

        // Print the output
        System.out.print("START");
        System.out.print("->");
        for(int i=0; i<outputStrings.size(); ++i){

            for(String str : outputStrings.get(i)){
                System.out.print(" " + str);
            }
            System.out.println();
            if(i != outputStrings.size()-1)
                System.out.print("\t ->");
        }

    }

    public ArrayList<String> reduceOneTime(ArrayList<String> tokenStrings, boolean statementSpec){
        ArrayList<String> temp = (ArrayList<String>) tokenStrings.clone();

        // This is not evaluated if statementSpec is true
        if(temp.size() == 1 && !statementSpec){
            // Ends the reducing START -> INPUT
            if(temp.get(0).equals("INPUT"))
                return null;

            // Takes care for INPUT -> EXPI | EXPLISTI
            if(temp.get(0).equals("EXPI") || temp.get(0).equals("EXPLISTI")) {
                temp.remove(0);
                temp.add("INPUT");
                return temp;
            }

            StringBuilder error = new StringBuilder();
            error.append("\nERROR[in 0]: Expected -> EXPI | EXPLISI\n");
            error.append("\t\t\tReceived  -> ");
            error.append(temp.get(0));
            throw new IllegalArgumentException(error.toString());
        }

        int FCPIndex = -1; // First Closing Paranthese index
        int LOPIndex = -1; // Last Openning Paranthese index,  the paranthese which openned the FCP
        int LLOPIndex = -1; // List Last Openning Paranthese index

        // Find the indices
        outerLoop:
        for(int i=0; i<temp.size(); ++i)
        {
            if(temp.get(i).equals("("))
            {
                LOPIndex = i;
            }
            if(temp.get(i).equals("'(")){
                LLOPIndex = i;
            }
            if(temp.get(i).equals(")")){
                FCPIndex = i;
                break;
            }
        }

        // The situation when the last openning paranthese before first closing is a list paranthese
        if(LLOPIndex != -1 && LLOPIndex > LOPIndex)
        {
            // Takes care of LISTVALUE -> '(VALUES)
            if(temp.get(LLOPIndex+1).equals("VALUES") && temp.get(LLOPIndex+2).equals(")")){
                temp.remove(LLOPIndex); // Remove '(
                temp.remove(LLOPIndex); // Remove VALUES
                temp.remove(LLOPIndex); // Remove )
                temp.add(LLOPIndex, "LISTVALUE"); // Place LISTVALUE
                return temp;
            }

            // Takes care of VALUES -> VALUES IntegerValue
            if(temp.get(LLOPIndex+1).equals("VALUES") && temp.get(LLOPIndex+2).equals("IntegerValue")){
                temp.remove(LLOPIndex+2); // Remove IntegerValue
                return temp;
            }

            // Takes care of VALUES -> IntegerValue
            if(temp.get(LLOPIndex+1).equals("IntegerValue")){
                temp.remove(LLOPIndex+1); // Remove IntegerValue
                temp.add(LLOPIndex+1, "VALUES"); // Place VALUES
                return temp;
            }

            // Takes care of LISTVALUE -> '()
            if(temp.get(LLOPIndex+1).equals(")")){
                temp.remove(LLOPIndex); // Remove '(
                temp.remove(LLOPIndex); // Remove )
                temp.add(LLOPIndex, "LISTVALUE"); // Place LISTVALUE
                return temp;
            }

            StringBuilder error = new StringBuilder();
            error.append("\nERROR:\n Expected -> '( VALUES ) | '() | '( VALUES IntegerValue ) | '( IntegerValue )\n");
            error.append("\t\t\tReceived  -> ");
            for(int e=LLOPIndex; e<FCPIndex; ++e){
                error.append(temp.get(e) + " ");
            }
            error.append(")");
            throw new IllegalArgumentException(error.toString());
        }

        // If there is an opened and closed parentheses
        if(LOPIndex != -1 && FCPIndex != -1)
        {
            // Takes care of EXPB -> BinaryValue
            // Takes care of EXPI -> IntegerValue | Id
            if(temp.get(LOPIndex+1).equals("and") || temp.get(LOPIndex+1).equals("or") ||
               temp.get(LOPIndex+1).equals("equal") || temp.get(LOPIndex+1).equals("not") ||
               temp.get(LOPIndex+1).equals("+") || temp.get(LOPIndex+1).equals("-") ||
               temp.get(LOPIndex+1).equals("*") || temp.get(LOPIndex+1).equals("/") ||
               temp.get(LOPIndex+1).equals("append") || temp.get(LOPIndex+1).equals("set"))
            {
                for(int i = LOPIndex+2; i<FCPIndex; ++i){
                    if(temp.get(i).equals("IntegerValue")){
                        temp.remove(i); // Remove IntegerValue
                        temp.add(i, "EXPI"); // Place EXPI
                        return temp;
                    }
                    else if(temp.get(i).equals("Id") && !temp.get(LOPIndex+1).equals("set")){
                        temp.remove(i); // Remove Id
                        temp.add(i, "EXPI"); // Place EXPI
                        return temp;
                    }
                    else if(temp.get(i).equals("BinaryValue")){
                        temp.remove(i); // Remove BinaryValue
                        temp.add(i, "EXPB"); // Place EXPB
                        return temp;
                    }
                }
            }

            // Takes care of IDLIST -> ( IDS )
            // Takes care of IDS -> IDS Id
            if(temp.get(LOPIndex+1).equals("IDS"))
            {
                if(temp.get(LOPIndex+2).equals(")")){
                    temp.subList(LOPIndex, FCPIndex+1).clear();
                    temp.add(LOPIndex, "IDLIST");
                    return temp;
                }
                else if(temp.get(LOPIndex+2).equals("Id")){
                    if(temp.get(LOPIndex+3).equals("Id") || temp.get(LOPIndex+3).equals(")")){
                        temp.remove(LOPIndex+2);
                        return temp;
                    }
                    else{
                        StringBuilder error = new StringBuilder();
                        error.append("\nERROR:\n Expected -> ( IDS Id )\n");
                        error.append("\t\t\tReceived  -> ");
                        error.append("(");
                        error.append(temp.get(LOPIndex+1) + " ");
                        error.append(temp.get(LOPIndex+2) + " ");
                        error.append(temp.get(LOPIndex+3));
                        throw new IllegalArgumentException(error.toString());
                    }
                }
                else{
                    StringBuilder error = new StringBuilder();
                    error.append("\nERROR:\n Expected -> ( IDS ) | ( IDS Id )\n");
                    error.append("\t\t\tReceived  -> ");
                    error.append("(");
                    error.append(temp.get(LOPIndex+1) + " ");
                    error.append(temp.get(LOPIndex+2) + " ");
                    error.append(temp.get(LOPIndex+3));
                    throw new IllegalArgumentException(error.toString());
                }
            }
            // Takes care of EXPI -> (Id EXPLISTI)
            else if(temp.get(LOPIndex+1).equals("Id"))
            {
                // Takes care of IDS -> Id
                if(temp.get(LOPIndex+2).equals(")") || temp.get(LOPIndex+2).equals("Id")){
                    temp.remove(LOPIndex+1);
                    temp.add(LOPIndex+1, "IDS");
                    return temp;
                }
                if(temp.get(LOPIndex+2).equals("EXPI")){
                    temp.subList(LOPIndex, FCPIndex+1).clear();
                    temp.add(LOPIndex, "EXPI");
                    return temp;
                }

                StringBuilder error = new StringBuilder();
                error.append("\nERROR:\n Expected -> ( IDS ) | ( IDS Id )\n");
                error.append("\t\t\tReceived  -> ");
                error.append("(");
                error.append(temp.get(LOPIndex+1) + " ");
                error.append(temp.get(LOPIndex+2) + " ");
                error.append(temp.get(LOPIndex+3));
                throw new IllegalArgumentException(error.toString());
            }

            // Takes care of EXPB -> (equal EXPB EXPB) | (equal EXPI EXPI) |
            // (and EXPB EXPB) | (or EXPB EXPB) | (not EXPB)
            if(temp.get(LOPIndex+1).equals("equal"))
            {
                if(((temp.get(LOPIndex+2).equals("EXPB") && temp.get(LOPIndex+3).equals("EXPB"))||
                    (temp.get(LOPIndex+2).equals("EXPI") && temp.get(LOPIndex+3).equals("EXPI"))) &&
                    temp.get(LOPIndex+4).equals(")"))
                {
                    temp.remove(LOPIndex); // Remove (
                    temp.remove(LOPIndex); // Remove equal
                    temp.remove(LOPIndex); // Remove EXPB or EXPI
                    temp.remove(LOPIndex); // Remove EXPB or EXPI
                    temp.remove(LOPIndex); // Remove )
                    temp.add(LOPIndex, "EXPB"); // Add EXPB
                    return temp;
                }
                else{
                    StringBuilder error = new StringBuilder();
                    error.append("\nERROR:\n Expected -> (equal EXPB EXPB) | (equal EXPI EXPI)\n");
                    error.append("\t\t\tReceived  -> ");
                    error.append("(");
                    error.append(temp.get(LOPIndex+1) + " ");
                    error.append(temp.get(LOPIndex+2) + " ");
                    error.append(temp.get(LOPIndex+3) + " ");
                    error.append(temp.get(LOPIndex+4));
                    throw new IllegalArgumentException(error.toString());
                }
            }
            else if(temp.get(LOPIndex+1).equals("and") || temp.get(LOPIndex+1).equals("or"))
            {
                if(temp.get(LOPIndex+2).equals("EXPB") && temp.get(LOPIndex+3).equals("EXPB") &&
                    temp.get(LOPIndex+4).equals(")"))
                {
                    temp.remove(LOPIndex); // Remove (
                    temp.remove(LOPIndex); // Remove And or Or
                    temp.remove(LOPIndex); // Remove EXPB
                    temp.remove(LOPIndex); // Remove EXPB
                    temp.remove(LOPIndex); // Remove )
                    temp.add(LOPIndex, "EXPB"); // Add EXPB
                    return temp;
                }
                else{
                    StringBuilder error = new StringBuilder();
                    error.append("\nERROR:\n Expected -> (" + temp.get(LOPIndex+1));
                    error.append(" EXPB EXPB)\n");
                    error.append("\t\t\tReceived  -> ");
                    error.append("(");
                    error.append(temp.get(LOPIndex+1) + " ");
                    error.append(temp.get(LOPIndex+2) + " ");
                    error.append(temp.get(LOPIndex+3) + " ");
                    error.append(temp.get(LOPIndex+4));
                    throw new IllegalArgumentException(error.toString());
                }
            }
            else if(temp.get(LOPIndex+1).equals("not"))
            {
                if((temp.get(LOPIndex+2).equals("EXPB")) &&
                    temp.get(LOPIndex+3).equals(")"))
                {
                    temp.remove(LOPIndex); // Remove (
                    temp.remove(LOPIndex); // Remove not
                    temp.remove(LOPIndex); // Remove EXPB
                    temp.remove(LOPIndex); // Remove )
                    temp.add(LOPIndex, "EXPB");
                    return temp;
                }
                else{
                    StringBuilder error = new StringBuilder();
                    error.append("\nERROR:\n Expected -> (not EXPB)\n");
                    error.append("\t\t\tReceived  -> ");
                    error.append("(");
                    error.append(temp.get(LOPIndex+1) + " ");
                    error.append(temp.get(LOPIndex+2) + " ");
                    error.append(temp.get(LOPIndex+3));
                    throw new IllegalArgumentException(error.toString());
                }
            }
            // Takes care of EXPI -> (+ EXPI EXPI) | (- EXPI EXPI) | (* EXPI EXPI) | (/ EXPI EXPI)
            else if(temp.get(LOPIndex+1).equals("+") || temp.get(LOPIndex+1).equals("-") ||
               temp.get(LOPIndex+1).equals("*") || temp.get(LOPIndex+1).equals("/"))
            {
                if(temp.get(LOPIndex+2).equals("EXPI") && temp.get(LOPIndex+3).equals("EXPI") &&
                    temp.get(LOPIndex+4).equals(")"))
                {
                    temp.subList(LOPIndex, FCPIndex+1).clear();
                    temp.add(LOPIndex, "EXPI");
                    return temp;
                }
                else{
                    StringBuilder error = new StringBuilder();
                    error.append("\nERROR:\n Expected -> (" + temp.get(LOPIndex+1));
                    error.append(" EXPI EXPI)\n");
                    error.append("\t\t\tReceived  -> ");
                    error.append("(");
                    error.append(temp.get(LOPIndex+1) + " ");
                    error.append(temp.get(LOPIndex+2) + " ");
                    error.append(temp.get(LOPIndex+3) + " ");
                    error.append(temp.get(LOPIndex+4));
                    throw new IllegalArgumentException(error.toString());
                }
            }

            // Takes care of EXPLISTI -> LISTVALUE
            for(int i = LOPIndex+1; i<FCPIndex; ++i)
                if(temp.get(i).equals("LISTVALUE")){
                    temp.remove(i); // Remove LISTVALUE
                    temp.add(i, "EXPLISTI"); // Place EXPLIST
                    return temp;
                }

            // Takes care of EXPLISTI -> (concat EXPLISTI EXPLISTI)
            if(temp.get(LOPIndex+1).equals("concat")){
                if(temp.get(LOPIndex+2).equals("EXPLISTI") && temp.get(LOPIndex+3).equals("EXPLISTI") &&
                    temp.get(LOPIndex+4).equals(")"))
                {
                    temp.subList(LOPIndex, FCPIndex+1).clear();
                    temp.add(LOPIndex, "EXPLISTI");
                    return temp;
                }
                else{
                    StringBuilder error = new StringBuilder();
                    error.append("\nERROR:\n Expected -> (concat EXPLISTI EXPLISI)\n");
                    error.append("\t\t\tReceived  -> ");
                    error.append("(concat ");
                    error.append(temp.get(LOPIndex+2) + " ");
                    error.append(temp.get(LOPIndex+3) + " ");
                    error.append(temp.get(LOPIndex+4));
                    throw new IllegalArgumentException(error.toString());
                }
            }

            // Takes care of EXPLISTI -> (append EXPI EXPLISTI)
            if(temp.get(LOPIndex+1).equals("append")){
                if(temp.get(LOPIndex+2).equals("EXPI") && temp.get(LOPIndex+3).equals("EXPLISTI") &&
                   temp.get(LOPIndex+4).equals(")"))
                {
                    temp.subList(LOPIndex, FCPIndex+1).clear();
                    temp.add(LOPIndex, "EXPLISTI");
                    return temp;
                }
                else{
                    StringBuilder error = new StringBuilder();
                    error.append("\nERROR:\n Expected -> (append EXPI EXPLISI)\n");
                    error.append("\t\t\tReceived  -> ");
                    error.append("(");
                    error.append(temp.get(LOPIndex+1) + " ");
                    error.append(temp.get(LOPIndex+2) + " ");
                    error.append(temp.get(LOPIndex+3) + " ");
                    error.append(temp.get(LOPIndex+4));
                    throw new IllegalArgumentException(error.toString());
                }
            }

            // Takes care of EXPI -> (defvar Id EXPI)
            if(temp.get(LOPIndex+1).equals("defvar")){
                if(temp.get(LOPIndex+2).equals("Id") && temp.get(LOPIndex+3).equals("EXPI") &&
                        temp.get(LOPIndex+4).equals(")"))
                {
                    temp.subList(LOPIndex, FCPIndex+1).clear();
                    temp.add(LOPIndex, "EXPI");
                    return temp;
                }
                else{
                    StringBuilder error = new StringBuilder();
                    error.append("\nERROR:\n Expected -> (defvar Id EXPI)\n");
                    error.append("\t\t\tReceived  -> ");
                    error.append("(");
                    error.append(temp.get(LOPIndex+1) + " ");
                    error.append(temp.get(LOPIndex+2) + " ");
                    error.append(temp.get(LOPIndex+3) + " ");
                    error.append(temp.get(LOPIndex+4));
                    throw new IllegalArgumentException(error.toString());
                }
            }

            // Takes care of EXPI -> (set Id EXPI)
            if(temp.get(LOPIndex+1).equals("set")){
                if(temp.get(LOPIndex+2).equals("Id") && temp.get(LOPIndex+3).equals("EXPI") &&
                        temp.get(LOPIndex+4).equals(")"))
                {
                    temp.subList(LOPIndex, FCPIndex+1).clear();
                    temp.add(LOPIndex, "EXPI");
                    return temp;
                }
                else{
                    StringBuilder error = new StringBuilder();
                    error.append("\nERROR:\n Expected -> (set Id EXPI)\n");
                    error.append("\t\t\tReceived  -> ");
                    error.append("(");
                    error.append(temp.get(LOPIndex+1) + " ");
                    error.append(temp.get(LOPIndex+2) + " ");
                    error.append(temp.get(LOPIndex+3) + " ");
                    error.append(temp.get(LOPIndex+4));
                    throw new IllegalArgumentException(error.toString());
                }
            }

            // Takes care of EXPI -> (deffun Id IDLIST EXPLISTI)
            if(temp.get(LOPIndex+1).equals("deffun"))
            {
                if(temp.get(LOPIndex+2).equals("Id") && temp.get(LOPIndex+3).equals("IDLIST") &&
                        temp.get(LOPIndex+4).equals("EXPI") && temp.get(LOPIndex+5).equals(")"))
                {
                    temp.subList(LOPIndex, FCPIndex+1).clear();
                    temp.add(LOPIndex, "EXPI");
                    return temp;
                }
                else{
                    StringBuilder error = new StringBuilder();
                    error.append("\nERROR:\n Expected -> ( deffun Id IDLIST EXPI )\n");
                    error.append("\t\t\tReceived  -> ");
                    error.append("(");
                    error.append(temp.get(LOPIndex+1) + " ");
                    error.append(temp.get(LOPIndex+2) + " ");
                    error.append(temp.get(LOPIndex+3) + " ");
                    error.append(temp.get(LOPIndex+4) + " ");
                    error.append(temp.get(LOPIndex+5));
                    throw new IllegalArgumentException(error.toString());
                }
            }

            // Takes care of EXPI -> (if EXPB EXPI) | (if EXPB EXPI EXPI)
            // Takes care of EXPI -> (if EXPB then EXPI else EXPI)
            if(temp.get(LOPIndex+1).equals("if"))
            {
                if(temp.get(LOPIndex+2).equals("EXPB") && temp.get(LOPIndex+3).equals("EXPI") &&
                   temp.get(LOPIndex+4).equals(")"))
                {
                    temp.subList(LOPIndex, FCPIndex+1).clear();
                    temp.add(LOPIndex, "EXPI");
                    return temp;
                }
                if(temp.get(LOPIndex+2).equals("EXPB") && temp.get(LOPIndex+3).equals("EXPI") &&
                   temp.get(LOPIndex+4).equals("EXPI") && temp.get(LOPIndex+5).equals(")"))
                {
                    temp.subList(LOPIndex, FCPIndex+1).clear();
                    temp.add(LOPIndex, "EXPI");
                    return temp;
                }
                if(temp.get(LOPIndex+2).equals("EXPB") && temp.get(LOPIndex+3).equals("then") &&
                   temp.get(LOPIndex+4).equals("EXPI") && temp.get(LOPIndex+5).equals("else") &&
                   temp.get(LOPIndex+6).equals("EXPI") && temp.get(LOPIndex+7).equals(")"))
                {
                    temp.subList(LOPIndex, FCPIndex+1).clear();
                    temp.add(LOPIndex, "EXPI");
                    return temp;
                }

            }

            // Takes care of EXPI -> (while EXPB EXPI)
            if(temp.get(LOPIndex+1).equals("while"))
            {
                if(temp.get(LOPIndex+2).equals("EXPB") && temp.get(LOPIndex+3).equals("EXPI") &&
                   (temp.get(LOPIndex+4).equals(")")))
                {
                    temp.subList(LOPIndex, FCPIndex+1).clear();
                    temp.add(LOPIndex, "EXPI");
                    return temp;
                }
            }

            // Takes care of EXPI -> (for Id EXPI EXPI EXPI)
            if(temp.get(LOPIndex+1).equals("for"))
            {
                if(temp.get(LOPIndex+2).equals("Id") && temp.get(LOPIndex+3).equals("EXPB") &&
                        temp.get(LOPIndex+4).equals("EXPI") && temp.get(LOPIndex+5).equals("EXPI") &&
                        temp.get(LOPIndex+6).equals(")"))
                {
                    temp.subList(LOPIndex, FCPIndex+1).clear();
                    temp.add(LOPIndex, "EXPI");
                    return temp;
                }
            }


            StringBuilder error = new StringBuilder();
            error.append("\nERROR: Unexpected parsing error occured! Please contact the developer!!\n");
            throw new IllegalArgumentException(error.toString());
        }
        return temp;
    }

    public ArrayList<String> produceTokenStrings(TokenList list){
        ArrayList<String> tempArr = new ArrayList<>();

        Token.Type currentTokenType;
        for(Token currToken : list.getAllTokens()) {
            currentTokenType = currToken.getTokenType();

            switch(currentTokenType){
                case IDENTIFIER:
                    tempArr.add("Id");
                    break;
                case OPERATOR:
                    Operator tempOp = (Operator) currToken;
                    tempArr.add(tempOp.getOperator());
                    break;
                case BINARY_VALUE:
                    tempArr.add("BinaryValue");
                    break;
                case INTEGER_VALUE:
                    tempArr.add("IntegerValue");
                    break;
                case KEYWORD:
                    Keyword tempK = (Keyword) currToken;
                    tempArr.add(tempK.getKeyword());
                    break;
            }
        }
        return tempArr;
    }

    public static boolean validateParentheses(ArrayList<String> tokenStrings)
    {
        Stack s = new Stack();
        String currentParenthese;
        String previousParenthese;
        boolean isValid = true;

        // Check all elements
        for(int i = 0; i < tokenStrings.size(); ++i)
        {
            // If we find parentheses element
            if(tokenStrings.get(i).equals("'(") || tokenStrings.get(i).equals("(") || tokenStrings.get(i).equals(")"))
            {
                currentParenthese = tokenStrings.get(i);

                // If the current element is opening parenthese
                if(currentParenthese.equals("(") || currentParenthese.equals("'("))
                {
                    s.push(currentParenthese);
                }
                // If the current element is closing parenthese
                else if(currentParenthese.equals(")"))
                {
                    // If the stack isn't containing any opening parenthese
                    if(s.isEmpty())
                    {
                        isValid = false;
                        break;
                    }
                    else // There are elements in the stack
                    {
                        // Get the last element of the stack
                        // It should be "(" or "'("
                        previousParenthese = (String)s.pop();
                        if(!(previousParenthese.equals("(") || previousParenthese.equals("'(")))
                        { // If not
                            isValid = false;
                            break;
                        }
                    }

                }

            }
        }

        // If there is any elements in the stack remaining
        if(!s.isEmpty())
            isValid = false;

        return isValid;
    }

    // Set all the rules of the coffee language for referance
    public void AddCoffeeRules(){

        // ----- EKSIK GORDUKLERIM BUNLAR BASLANGIC -----
        // IDLIST -> ( IDS )
        coffeeRules.add("( IDS )");
        // IDS -> IDS Id | Id
        coffeeRules.add("( IDS Id )");
        // ( IDS ) - > ( Id )
        coffeeRules.add("( Id )");
        // EXPLISTI -> EXPI
        coffeeRules.add(" EXPI ");

        // ----- EKSIK GORDUKLERIM BUNLAR SON -----

        /* Start Rules */
        // START -> INPUT
        coffeeRules.add(" START ");
        // INPUT -> EXPI | EXPLISTI  iff only one element is in the array
        coffeeRules.add(" INPUT ");

        /* Input Rules */
        // EXPLISTI -> EXPI
        coffeeRules.add(" EXPI ");
        coffeeRules.add(" EXPB "); // Eksikti ben ekledim
        coffeeRules.add(" EXPLISTI ");

        /* EXPI Rules */
        // Expressions
        // EXPI -> ( + EXPI EXPI )
        coffeeRules.add("( + EXPI EXPI )");
        // EXPI -> ( - EXPI EXPI )
        coffeeRules.add("( - EXPI EXPI )");
        // EXPI -> ( * EXPI EXPI )
        coffeeRules.add("( * EXPI EXPI )");
        // EXPI -> ( / EXPI EXPI )
        coffeeRules.add("( / EXPI EXPI )");
        // EXPI -> Id
        coffeeRules.add(" Id ");
        // VALUES -> IntegerValue also EXPI -> IntegerValue CONFLICT HERE!!!!!!
        coffeeRules.add(" IntegerValue ");
        // Assignment
        // EXPI -> ( set Id EXPI )
        coffeeRules.add("( set Id EXPI )");
        // Function Definition
        // EXPI -> ( deffun Id IDLIST EXPLISTI )
        coffeeRules.add("( deffun Id IDLIST EXPLISTI )");
        // Function Call
        // EXPI -> ( Id EXPI )
        coffeeRules.add("( Id EXPI )");
        // Control Statements
        // The following 6 reduces to EXPI ->
        coffeeRules.add("( if EXPB then EXPI else EXPI )");
        coffeeRules.add("( if EXPB EXPI )");
        coffeeRules.add("( if EXPB EXPI EXPI )");
        coffeeRules.add("( while EXPB EXPI )");
        coffeeRules.add("( for Id EXPB EXPI EXPI )");

        // Variable Defining
        // EXPI -> ( defvar Id EXPI )
        coffeeRules.add("( defvar Id EXPI )");
        // Variable Setting
        // EXPI -> ( set Id EXPI )
        coffeeRules.add("( set Id EXPI )");

        /* EXPB Rules */
        // Expressions
        // The following 6 reduces to EXPB ->
        coffeeRules.add("( and EXPB EXPB )");
        coffeeRules.add("( or EXPB EXPB )");
        coffeeRules.add("( not EXPB )");
        coffeeRules.add("( equal EXPB EXPB )");
        coffeeRules.add("( equal EXPI EXPI )");
        coffeeRules.add(" BinaryValue ");

        /* EXPLISTI Rules */
        // Expressions
        // EXPLISTI -> concat, append, LISTVALUE
        coffeeRules.add("( concat EXPLISTI EXPLISTI )");
        coffeeRules.add("( append EXPI EXPLISTI )");
        coffeeRules.add(" LISTVALUE ");
        // EXPLISTI -> null also LISTVALUE -> null
        coffeeRules.add(" null ");

        /* List Rules */
        // LISTVALUE -> '( VALUES ), '( ), null
        coffeeRules.add(" '( VALUES ) ");
        coffeeRules.add(" '( ) ");
        //coffeeRules.add(" null "); // Already added
        coffeeRules.add(" VALUES IntegerValue ");
        //coffeeRules.add(" IntegerValue "); //Already added

    }
}
