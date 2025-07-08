// Classic diamond inheritance problem
class DiamondBase {
public:
    virtual void diamondMethod() {}
    virtual ~DiamondBase() {}
    
private:
    int diamondData;
};

class DiamondLeft : virtual public DiamondBase {
public:
    virtual void leftMethod() {}
    virtual void diamondMethod() override {}
    virtual ~DiamondLeft() {}
    
private:
    double leftData;
};

class DiamondRight : virtual public DiamondBase {
public:
    virtual void rightMethod() {}
    virtual void diamondMethod() override {}
    virtual ~DiamondRight() {}
    
private:
    char rightData;
};

class DiamondBottom : public DiamondLeft, public DiamondRight {
public:
    virtual void bottomMethod() {}
    virtual void diamondMethod() override {}
    virtual ~DiamondBottom() {}
    
private:
    float bottomData;
}; 