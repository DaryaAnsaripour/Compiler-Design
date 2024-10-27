package main.visitor.type;

import main.ast.nodes.Program;
import main.ast.nodes.declaration.*;
import main.ast.nodes.expression.*;
import main.ast.nodes.expression.operators.*;
import main.ast.nodes.expression.value.*;
import main.ast.nodes.expression.value.primitive.*;
import main.ast.nodes.statement.*;
import main.ast.type.*;
import main.ast.type.primitiveType.*;
import main.compileError.CompileError;
import main.compileError.typeErrors.*;
import main.symbolTable.SymbolTable;
import main.symbolTable.exceptions.*;
import main.symbolTable.item.*;
import main.visitor.Visitor;

import java.util.*;

public class TypeChecker extends Visitor<Type> {
    public Set<Type> returnTypes = new HashSet<Type>();
    public ArrayList<CompileError> typeErrors = new ArrayList<>();
    @Override
    public Type visit(Program program){
        SymbolTable.root = new SymbolTable();
        SymbolTable.top = new SymbolTable();
        for(FunctionDeclaration functionDeclaration : program.getFunctionDeclarations()){
            FunctionItem functionItem = new FunctionItem(functionDeclaration);
            try {
                SymbolTable.root.put(functionItem);
            }catch (ItemAlreadyExists ignored){}
        }
        for(PatternDeclaration patternDeclaration : program.getPatternDeclarations()){
            PatternItem patternItem = new PatternItem(patternDeclaration);
            try{
                SymbolTable.root.put(patternItem);
            }catch (ItemAlreadyExists ignored){}
        }
        program.getMain().accept(this);

        return null;
    }
    @Override
    public Type visit(FunctionDeclaration functionDeclaration){
        SymbolTable.push(new SymbolTable());
        try {
            FunctionItem functionItem = (FunctionItem) SymbolTable.root.getItem(FunctionItem.START_KEY +
                    functionDeclaration.getFunctionName().getName());
            ArrayList<Type> currentArgTypes = functionItem.getArgumentTypes();
            for (int i = 0; i < functionDeclaration.getArgs().size(); i++) {
                VarItem argItem = new VarItem(functionDeclaration.getArgs().get(i).getName());
                argItem.setType(currentArgTypes.get(i));

                try {
                    SymbolTable.top.put(argItem);
                }catch (ItemAlreadyExists ignored){}
            }
        }catch (ItemNotFound ignored){}
        for(Statement statement : functionDeclaration.getBody()){
            statement.accept(this);
        }
            

        //////////////////TODO:Figure out whether return types of functions are not the same.//////////////////
        // ArrayList<Type> returnTypes = new ArrayList<>();
        // for(Statement statement : functionDeclaration.getBody()){
        //     if(statement instanceof ReturnStatement){
        //         returnTypes.add(((ReturnStatement) statement).getReturnExp().accept(this));
        //     }
        // }
        // Type first_type = returnTypes.get(0);
        // for(Type type : returnTypes)
        //     if(!first_type.sameType(type)){
        //         typeErrors.add(new FunctionIncompatibleReturnTypes(functionDeclaration.getLine(), functionDeclaration.getFunctionName().getName()));
        //         SymbolTable.pop();
        //         return new NoType();
        //     }
        SymbolTable.pop();
        if (returnTypes.size() > 1){
            typeErrors.add(new FunctionIncompatibleReturnTypes(functionDeclaration.getLine(), functionDeclaration.getFunctionName().getName()));
            return new NoType();
        }
        //////////////////TODO:Return the infered type of the function.//////////////////
        Type type = returnTypes.iterator().next();
        returnTypes.clear();
        return type;
    }
    @Override
    public Type visit(PatternDeclaration patternDeclaration){
        SymbolTable.push(new SymbolTable());
        try {
            PatternItem patternItem = (PatternItem) SymbolTable.root.getItem(PatternItem.START_KEY +
                    patternDeclaration.getPatternName().getName());
            VarItem varItem = new VarItem(patternDeclaration.getTargetVariable());
            varItem.setType(patternItem.getTargetVarType());
            try {
                SymbolTable.top.put(varItem);
            }catch (ItemAlreadyExists ignored){}
            for(Expression expression : patternDeclaration.getConditions()){
                if(!(expression.accept(this) instanceof BoolType)){
                    typeErrors.add(new ConditionIsNotBool(expression.getLine()));
                    SymbolTable.pop();
                    return new NoType();
                }
            }
        //TODO:1-figure out whether return expression of different cases in pattern are of the same type/2-return the infered type.
        ArrayList<Type> returnTypes = new ArrayList<>();
        ArrayList<Expression> expressions = patternDeclaration.getReturnExp();
        for(Expression expression : expressions)
            returnTypes.add(expression.accept(this));

        Type first_type = returnTypes.get(0);

        for(Type type : returnTypes)
            if(!first_type.sameType(type)){
                typeErrors.add(new PatternIncompatibleReturnTypes(patternDeclaration.getLine(), patternDeclaration.getPatternName().getName()));
                SymbolTable.pop();
                return new NoType();
            }
        SymbolTable.pop();
        return first_type;
        }catch (ItemNotFound ignored){}
        return new NoType();
    }
    @Override
    public Type visit(MainDeclaration mainDeclaration){
        //////////////////TODO:visit main.//////////////////
        SymbolTable.push(new SymbolTable());
        ArrayList<Statement> statements = mainDeclaration.getBody();
        for(Statement statement : statements)
            statement.accept(this);
        SymbolTable.pop();
        return new NoType();
    }
    @Override
    public Type visit(AccessExpression accessExpression){
        if(accessExpression.isFunctionCall()){
            //TODO:function is called here.set the arguments type and visit its declaration.
            ArrayList<Expression> arguments = accessExpression.getArguments();
            ArrayList<Type> argTypes = new ArrayList<>();
            for(Expression expression : arguments)
                argTypes.add(expression.accept(this));

            Identifier functionName = (Identifier) accessExpression.getAccessedExpression();
            try {
                FunctionItem functionItem = (FunctionItem) SymbolTable.root.getItem(FunctionItem.START_KEY + functionName.getName());
                for(int i = 0; i < functionItem.getFunctionDeclaration().getArgs().size(); i++){
                    if(functionItem.getFunctionDeclaration().getArgs().get(i).getDefaultVal() != null){
                        Type t = functionItem.getFunctionDeclaration().getArgs().get(i).getDefaultVal().accept(this);
                        argTypes.add(t);
                    }
                }
                functionItem.setArgumentTypes(argTypes);
                return functionItem.getFunctionDeclaration().accept(this);
            }catch (ItemNotFound ignored){}
        }
        else{
            Type accessedType = accessExpression.getAccessedExpression().accept(this);
            if(!(accessedType instanceof StringType) && !(accessedType instanceof ListType)){
                typeErrors.add(new IsNotIndexable(accessExpression.getLine()));
                return new NoType();
            }
            //////////////////TODO:index of access list must be int.////////////////// 
            for(Expression expression : accessExpression.getDimentionalAccess())
                if(!(expression.accept(this) instanceof IntType)){
                    typeErrors.add(new AccessIndexIsNotInt(accessExpression.getLine()));
                    return new NoType();
                }
            if(accessedType instanceof StringType)
                return new StringType();
            else
                return ((ListType) accessedType).getType();
        }
        return null;
    }

    @Override
    public Type visit(ReturnStatement returnStatement){
        //////////////////TODO:Visit return statement.Note that return type of functions are specified here.//////////////////
        Type type = returnStatement.getReturnExp().accept(this);
        returnTypes.add(type);
        return type;
    }
    @Override
    public Type visit(ExpressionStatement expressionStatement){
        return expressionStatement.getExpression().accept(this);
    }
    @Override
    public Type visit(ForStatement forStatement){
        SymbolTable.push(SymbolTable.top.copy());
        VarItem varItem = new VarItem(forStatement.getIteratorId());
        Type type = forStatement.getRangeExpression().accept(this);
        if(type instanceof StringType){
            varItem.setType(new StringType());
        }
        else if(type instanceof ListType){
            varItem.setType(((ListType) type).getType());
        }
        else if(type instanceof NoType){
            varItem.setType(new NoType());
        }
        try{
            SymbolTable.top.put(varItem);
        }catch (ItemAlreadyExists ignored){}

        for(Statement statement : forStatement.getLoopBodyStmts())
            statement.accept(this);
        SymbolTable.pop();
        return null;
    }
    @Override
    public Type visit(IfStatement ifStatement){
        SymbolTable.push(SymbolTable.top.copy());
        for(Expression expression : ifStatement.getConditions())
            if(!(expression.accept(this) instanceof BoolType))
                typeErrors.add(new ConditionIsNotBool(expression.getLine()));
        for(Statement statement : ifStatement.getThenBody())
            statement.accept(this);
        for(Statement statement : ifStatement.getElseBody())
            statement.accept(this);
        SymbolTable.pop();
        return new NoType();
    }
    @Override
    public Type visit(LoopDoStatement loopDoStatement){
        SymbolTable.push(SymbolTable.top.copy());
        for(Statement statement : loopDoStatement.getLoopBodyStmts())
            statement.accept(this);
        SymbolTable.pop();
        return new NoType();
    }
    @Override
    public Type visit(AssignStatement assignStatement){
        if(assignStatement.isAccessList()){
            // TODO:assignment to list
            Identifier assignedId = assignStatement.getAssignedId();
            Expression assignExpression = assignStatement.getAssignExpression();
            Expression accessListExpression = assignStatement.getAccessListExpression();
            Type assignedIdType = null;
            try {
                assignedIdType = ((VarItem) SymbolTable.top.getItem(VarItem.START_KEY + assignedId.getName())).getType();
            }catch (ItemNotFound ignored){}
            Type accessListType = accessListExpression.accept(this);
            if(!(accessListType instanceof IntType)){
                typeErrors.add(new AccessIndexIsNotInt(accessListExpression.getLine()));
                return new NoType();
            }
            Type assignType = assignExpression.accept(this);
            if(!((ListType) assignedIdType).getType().sameType(assignType)){
                typeErrors.add(new ListElementsTypesMisMatch(assignStatement.getLine()));
                return new NoType();
            }
            return new NoType();
        }
        else{
            VarItem newVarItem = new VarItem(assignStatement.getAssignedId());
            // TODO:maybe new type for a variable.
            Expression assExpression = assignStatement.getAssignExpression();
            AssignOperator assignOperator = assignStatement.getAssignOperator();
            Type assType = assExpression.accept(this);
            if(assignOperator == AssignOperator.ASSIGN){
                newVarItem.setType(assType);
            }
            else{
                Type varType = null;
                try {
                    varType = ((VarItem) SymbolTable.top.getItem(VarItem.START_KEY + assignStatement.getAssignedId().getName())).getType();
                    if(varType instanceof IntType && assType instanceof IntType){
                        newVarItem.setType(new IntType());
                    }
                    else if(varType instanceof FloatType && assType instanceof FloatType){
                        newVarItem.setType(new FloatType());
                    }
                    else if(varType instanceof FloatType && assType instanceof IntType){
                        newVarItem.setType(new FloatType());
                    }
                    else{
                        typeErrors.add(new UnsupportedOperandType(assignStatement.getLine(), assignOperator.toString()));
                        return new NoType();
                    }
                }catch (ItemNotFound ignored){}
            }
            try {
                SymbolTable.top.put(newVarItem);
            }catch (ItemAlreadyExists ignored){}
        }
        return new NoType();
    }
    @Override
    public Type visit(BreakStatement breakStatement){
        for(Expression expression : breakStatement.getConditions())
            if(!((expression.accept(this)) instanceof BoolType))
                typeErrors.add(new ConditionIsNotBool(expression.getLine()));

        return null;
    }
    @Override
    public Type visit(NextStatement nextStatement){
        for(Expression expression : nextStatement.getConditions())
            if(!((expression.accept(this)) instanceof BoolType))
                typeErrors.add(new ConditionIsNotBool(expression.getLine()));

        return null;
    }
    @Override
    public Type visit(PushStatement pushStatement){
        //TODO:visit push statement.
        Expression initial = pushStatement.getInitial();
        Expression toBeAdded = pushStatement.getToBeAdded();
        Type initialType = initial.accept(this);
        Type toBeAddedType = toBeAdded.accept(this);

        if(!(initialType instanceof ListType) && !(initialType instanceof StringType)){
            typeErrors.add(new IsNotPushedable(initial.getLine()));
            return new NoType();
        }

        if(initialType instanceof ListType){
            if(!((ListType) initialType).getType().sameType(toBeAddedType)){
                typeErrors.add(new PushArgumentsTypesMisMatch(pushStatement.getLine()));
                return new NoType();
            }
        }

        if(initialType instanceof StringType && !(toBeAddedType instanceof StringType)){
            typeErrors.add(new PushArgumentsTypesMisMatch(pushStatement.getLine()));
            return new NoType();
        }

        return new NoType();
    }
    @Override
    public Type visit(PutStatement putStatement){
        //TODO:visit putStatement.
        putStatement.getExpression().accept(this);
        return new NoType();

    }
    @Override
    public Type visit(BoolValue boolValue){
        return new BoolType();
    }
    @Override
    public Type visit(IntValue intValue){
        return new IntType();
    }
    @Override
    public Type visit(FloatValue floatValue){return new FloatType();}
    @Override
    public Type visit(StringValue stringValue){
        return new StringType();
    }
    @Override
    public Type visit(ListValue listValue){
        // TODO:visit listValue.
        ArrayList<Expression> elements = listValue.getElements();
        ArrayList<Type> elementTypes = new ArrayList<>();
        for(Expression expression : elements)
            elementTypes.add(expression.accept(this));
        
        if(elementTypes.size() == 0)
            return new ListType(new NoType());
        Type firstType = elementTypes.get(0);
        for(Type type : elementTypes)
            if(!firstType.sameType(type)){
                typeErrors.add(new ListElementsTypesMisMatch(listValue.getLine()));
                return new NoType();
            }
        return new ListType(firstType);
    }
    @Override
    public Type visit(FunctionPointer functionPointer){
        return new FptrType(functionPointer.getId().getName());
    }
    @Override
    public Type visit(AppendExpression appendExpression){
        Expression appendee = appendExpression.getAppendee();
        ArrayList<Expression> appendeds = appendExpression.getAppendeds();
        Type appendeeType = appendee.accept(this);
        if(appendeeType instanceof StringType){
            for(Expression appended : appendeds){
                if(!(appended.accept(this) instanceof StringType)){
                    typeErrors.add(new AppendTypesMisMatch(appendExpression.getLine()));
                    return new NoType();
                }
            }
            return new StringType();
        }
        else if(appendeeType instanceof ListType){
            for(Expression appended : appendeds){
                if(!((ListType) appendeeType).getType().sameType(appended.accept(this))){
                    typeErrors.add(new AppendTypesMisMatch(appendExpression.getLine()));
                    return new NoType();
                }
            }
            return appendeeType;
        }
        else{
            typeErrors.add(new UnsupportedOperandType(appendExpression.getLine(), "append"));
            return new NoType();
        }
    }
    @Override
    public Type visit(BinaryExpression binaryExpression){
        //TODO:visit binary expression.
        BinaryOperator operator = binaryExpression.getOperator();
        Expression firstOperand = binaryExpression.getFirstOperand();
        Expression secondOperand = binaryExpression.getSecondOperand();

        if(operator == BinaryOperator.DIVIDE || operator == BinaryOperator.MINUS || operator == BinaryOperator.MULT || operator == BinaryOperator.PLUS){
            Type firstType = firstOperand.accept(this);
            Type secondType = secondOperand.accept(this);

            if(operator == BinaryOperator.DIVIDE || operator == BinaryOperator.MINUS || operator == BinaryOperator.MULT || operator == BinaryOperator.PLUS){
                if(firstType instanceof IntType && secondType instanceof IntType)
                    return new IntType();
                else if(firstType instanceof FloatType && secondType instanceof FloatType)
                    return new FloatType();
                else if(firstType instanceof IntType && secondType instanceof NoType)
                    return new IntType();
                else if(firstType instanceof NoType && secondType instanceof IntType)
                    return new IntType();
                    else if(firstType instanceof NoType && secondType instanceof FloatType)
                    return new FloatType();
                else if(firstType instanceof FloatType && secondType instanceof NoType)
                    return new FloatType();
                else{
                    typeErrors.add(new NonSameOperands(binaryExpression.getLine(), operator));
                    return new NoType();
                }
            }
        }
        else if(operator == BinaryOperator.GREATER_EQUAL_THAN || operator == BinaryOperator.GREATER_THAN 
                                || operator == BinaryOperator.LESS_EQUAL_THAN || operator == BinaryOperator.LESS_THAN){
            Type firstType = firstOperand.accept(this);
            Type secondType = secondOperand.accept(this);

            if(firstType instanceof IntType && secondType instanceof IntType)
                return new BoolType();
            else if(firstType instanceof FloatType && secondType instanceof FloatType)
                return new BoolType();
            else if(firstType instanceof IntType && secondType instanceof NoType)
                return new BoolType();
            else if(firstType instanceof NoType && secondType instanceof IntType)
                return new BoolType();
            else if(firstType instanceof NoType && secondType instanceof FloatType)
                return new BoolType();
            else if(firstType instanceof FloatType && secondType instanceof NoType)
                return new BoolType();
            else if(firstType instanceof BoolType && secondType instanceof BoolType)
                return new BoolType();
            else if(firstType instanceof StringType && secondType instanceof StringType)
                return new BoolType();
            else if(firstType instanceof ListType && secondType instanceof ListType)
                return new BoolType();
            else{
                typeErrors.add(new NonSameOperands(binaryExpression.getLine(), operator));
                return new NoType();
            }
        }
        else if(operator == BinaryOperator.EQUAL || operator == BinaryOperator.NOT_EQUAL){
            Type firstType = firstOperand.accept(this);
            Type secondType = secondOperand.accept(this);

            if(firstType instanceof IntType && secondType instanceof IntType)
                return new BoolType();
            else if(firstType instanceof FloatType && secondType instanceof FloatType)
                return new BoolType();
            else if(firstType instanceof IntType && secondType instanceof NoType)
                return new BoolType();
            else if(firstType instanceof NoType && secondType instanceof IntType)
                return new BoolType();
            else if(firstType instanceof FloatType && secondType instanceof NoType)
                return new BoolType();
            else if(firstType instanceof NoType && secondType instanceof FloatType)
                return new BoolType();
            else if(firstType instanceof BoolType && secondType instanceof BoolType)
                return new BoolType();
            else if(firstType instanceof StringType && secondType instanceof StringType)
                return new BoolType();
            else if(firstType instanceof ListType && secondType instanceof ListType)
                return new BoolType();
            else{
                typeErrors.add(new NonSameOperands(binaryExpression.getLine(), operator));
                return new NoType();
            }
        }
        return null;
    }
    @Override
    public Type visit(UnaryExpression unaryExpression){
        //TODO:visit unaryExpression.
        UnaryOperator operator = unaryExpression.getOperator();
        Expression operand = unaryExpression.getExpression();

        if(operator == UnaryOperator.MINUS || operator == UnaryOperator.INC || operator == UnaryOperator.DEC){
            Type operandType = operand.accept(this);
            if(operandType instanceof IntType || operandType instanceof NoType)
                return new IntType();
            else if(operandType instanceof FloatType || operandType instanceof NoType)
                return new FloatType();
            else{
                typeErrors.add(new UnsupportedOperandType(unaryExpression.getLine(), operator.toString()));
                return new NoType();
            }
        }
        else if(operator == UnaryOperator.NOT){
            Type operandType = operand.accept(this);
            if(operandType instanceof BoolType || operandType instanceof NoType)
                return new BoolType();
            else{
                typeErrors.add(new UnsupportedOperandType(unaryExpression.getLine(), operator.toString()));
                return new NoType();
            }
        }
        return null;
    }
    @Override
    public Type visit(ChompStatement chompStatement){
        if (!(chompStatement.getChompExpression().accept(this) instanceof StringType)) {
            typeErrors.add(new ChompArgumentTypeMisMatch(chompStatement.getLine()));
            return new NoType();
        }

        return new StringType();
    }
    @Override
    public Type visit(ChopStatement chopStatement){
        if (!(chopStatement.getChopExpression().accept(this) instanceof StringType)) {
            typeErrors.add(new ChopArgumentTypeMisMatch(chopStatement.getLine()));
            return new NoType();
        }
        return new StringType();
    }
    @Override
    public Type visit(Identifier identifier){
        // TODO:visit Identifier. 
        try {
            VarItem varItem = (VarItem) SymbolTable.top.getItem(VarItem.START_KEY + identifier.getName());
            return varItem.getType();
        }catch (ItemNotFound ignored){}
        return null;
    }
    @Override
    public Type visit(LenStatement lenStatement){
        //TODO:visit LenStatement.Be carefull about the return type of LenStatement. 
        Expression lenexpression = lenStatement.getExpression();
        Type lenType = lenexpression.accept(this);
        if(!(lenType instanceof ListType) && !(lenType instanceof StringType)){
            typeErrors.add(new LenArgumentTypeMisMatch(lenStatement.getLine()));
            return new NoType();
        }
        return new IntType();
    }
    @Override
    public Type visit(MatchPatternStatement matchPatternStatement){
        try{
            PatternItem patternItem = (PatternItem)SymbolTable.root.getItem(PatternItem.START_KEY +
                    matchPatternStatement.getPatternId().getName());
            patternItem.setTargetVarType(matchPatternStatement.getMatchArgument().accept(this));
            return patternItem.getPatternDeclaration().accept(this);
        }catch (ItemNotFound ignored){}
        return new NoType();
    }
    @Override
    public Type visit(RangeExpression rangeExpression){
        RangeType rangeType = rangeExpression.getRangeType();
        ArrayList<Expression> rangeExpressions = rangeExpression.getRangeExpressions();

        if(rangeType.equals(RangeType.LIST)){
            // TODO --> mind that the lists are declared explicitly in the grammar in this node, so handle the errors. 
            Type firstType = rangeExpressions.get(0).accept(this);
            for(Expression expression : rangeExpressions)
                if(!expression.accept(this).sameType(firstType)){
                    typeErrors.add(new ListElementsTypesMisMatch(rangeExpression.getLine()));
                    return new NoType();
                }
            return new ListType(firstType);
        }
        else if(rangeType.equals(RangeType.DOUBLE_DOT)){
            Type firstType = rangeExpressions.get(0).accept(this);
            Type secondType = rangeExpressions.get(1).accept(this);
            if(firstType instanceof IntType && secondType instanceof IntType)
            {
                return new ListType(new IntType());
            }
            else{
                typeErrors.add(new RangeValuesMisMatch(rangeExpression.getLine()));
                return new NoType();
            }
        }
        else if(rangeType.equals(RangeType.IDENTIFIER)){
            Type idType = rangeExpressions.get(0).accept(this);
            if(!(idType instanceof ListType)){
                typeErrors.add(new IsNotIterable(rangeExpression.getLine()));
                return new NoType();
            }
            return idType;
        }
        return new NoType();
    }
}
