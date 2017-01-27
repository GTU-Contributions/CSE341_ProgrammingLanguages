package coffee.project;

import coffee.REPL;
import coffee.datatypes.*;
import coffee.syntax.*;
import coffee.IdentifierList;
import coffee.TokenList;

import java.util.StringTokenizer;
import java.util.ArrayList;

/**
 * Created by ft on 10/14/15.
 * Implemented by Mehmed MUSTAFA 131044084
 */
public class Lexer implements REPL.LineInputCallback {
    @Override
    public String lineInput(String line) throws IllegalTokenException {
        boolean DEBBUG = false;

        // Returning delimiters as tokens is set to true
        // " ()" are the delimiters
        StringTokenizer tokenizer = new StringTokenizer(line, " ()", true);
        ArrayList<String> tempTokens = new ArrayList<>();

        while(tokenizer.hasMoreTokens()){
            String currentToken = tokenizer.nextToken();
            // If the current token is whitespace continue
            if(currentToken.equals(" ") || currentToken.equals("\t") || currentToken.equals("\n"))
                continue;

            // Else add the token to tempTokens list
            tempTokens.add(currentToken);
        }

        if(DEBBUG){
            System.out.format("Tokens: ");
            for(int i = 0; i < tempTokens.size(); ++i) {
                System.out.format("[" + tempTokens.get(i) + "]");
            }
            System.out.format("\n");
        }

        for(String currentToken : tempTokens){
            // Check if the token is operator
            if(isOperator(currentToken)){
                TokenList.getInstance().addToken(new Operator(currentToken));
            }
            // Check if the token is keyword
            else if(isKeyword(currentToken)){
                // Check if the token is binary value
                if(isBinary(currentToken)) {
                    if (currentToken.equals(Keywords.TRUE))
                        TokenList.getInstance().addToken(new ValueBinary(true));
                    else
                        TokenList.getInstance().addToken(new ValueBinary(false));
                }
                else
                    TokenList.getInstance().addToken(new Keyword(currentToken));
            }
            // Check if the token is identifier
            else if(isIdentifier(currentToken)){
                IdentifierList.getInstance().addIdentifier(currentToken);
            }
            // Check if the token is integer value
            else if(isInteger(currentToken)){
                TokenList.getInstance().addToken(new ValueInt(Integer.parseInt(currentToken)));
            }
            // Illegal token
            else
                throw new IllegalTokenException(
                    String.format("Token[%s] is illegal!", currentToken)
                );
        }

        return null;
    }

    /**
     * Checks if the testToken is an operator
     * @param testToken token to test
     * @return true if the token is operator
     */
    public boolean isOperator(String testToken){
        if (testToken.equals(Operators.PLUS)             ||
            testToken.equals(Operators.MINUS)            ||
            testToken.equals(Operators.SLASH)            ||
            testToken.equals(Operators.ASTERISK)         ||
            testToken.equals(Operators.RIGHT_PARENTHESIS)||
            testToken.equals(Operators.LEFT_PARENTHESIS) ||
            testToken.equals(Operators.APOSTROPHE))
            return true;

        return false;
    }

    /**
     * Checks if the testToken is a keyword
     * @param testToken token to test
     * @return true if the token is keyword
     */
    public boolean isKeyword(String testToken){
        if (testToken.equals(Keywords.IF)       ||
            testToken.equals(Keywords.OR)       ||
            testToken.equals(Keywords.AND)      ||
            testToken.equals(Keywords.NOT)      ||
            testToken.equals(Keywords.FOR)      ||
            testToken.equals(Keywords.SET)      ||
            testToken.equals(Keywords.THEN)     ||
            testToken.equals(Keywords.ELSE)     ||
            testToken.equals(Keywords.TRUE)     ||
            testToken.equals(Keywords.FALSE)    ||
            testToken.equals(Keywords.EQUAL)    ||
            testToken.equals(Keywords.WHILE)    ||
            testToken.equals(Keywords.CONCAT)   ||
            testToken.equals(Keywords.DEFFUN)   ||
            testToken.equals(Keywords.APPEND))
            return true;

        return false;
    }

    /**
     * Checks if the testToken is a binary
     * @param testToken token to test
     * @return true if the token is binary
     */
    public boolean isBinary(String testToken){
        if (testToken.equals(Keywords.TRUE) ||
            testToken.equals(Keywords.FALSE))
            return true;

        return false;
    }

    /**
     * Checks if the testToken is an identifier
     * @param testToken token to test
     * @return true if the token is identifier
     */
    public boolean isIdentifier(String testToken){
        for(int i=0; i<testToken.length(); ++i)
            if(!Character.isLetter(testToken.charAt(i)))
                return false;

        return true;
    }

    /**
     * Checks if the testToken is an integer
     * @param testToken token to test
     * @return true if the token is integer
     */
    public boolean isInteger(String testToken){
        if(testToken.length() < 1)
            return false;

        // If the token's size is 1
        if(testToken.length() == 1){
            // If the token is not a digit
            if(!Character.isDigit(testToken.charAt(0)))
                return false;
        }
        // If the token's size is bigger than 1
        else{
            // If the first char is a minus
            if(testToken.charAt(0) == Operators.MINUS.charAt(0)){
                // If the char after minus is 0 or the char is not a digit
                if(testToken.charAt(1) == '0' || !Character.isDigit(testToken.charAt(1)))
                    return false;

                // Check if all characters after the first two chars are digits
                for(int i=2; i<testToken.length(); ++i)
                    if(!Character.isDigit(testToken.charAt(i)))
                        return false;
            }
            // If the first char is not a minus
            else{
                // If the first char is 0 or the first char is not a digit
                if(testToken.charAt(0) == '0' || !Character.isDigit(testToken.charAt(1)))
                    return false;

                // Check if all characters after the first char are digits
                for(int i=1; i<testToken.length(); ++i)
                    if(!Character.isDigit(testToken.charAt(i)))
                        return false;
            }
        }

        return true;
    }

    /**
     * Illegal Token Exception
     */
    public static class IllegalTokenException extends Exception{
        private String message;

        public IllegalTokenException(String message){
            super(message);
        }
    }
}
