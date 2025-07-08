// Complex inheritance with mixed virtual and non-virtual inheritance
class X {
public:
    virtual void xMethod() {}
    virtual ~X() {}
    
private:
    int xData;
};

class Y1 : public X {
public:
    virtual void y1Method() {}
    virtual void xMethod() override {}
    virtual ~Y1() {}
    
private:
    double y1Data;
};

class Y2 : virtual public X {
public:
    virtual void y2Method() {}
    virtual void xMethod() override {}
    virtual ~Y2() {}
    
private:
    char y2Data;
};

class Z : public Y1, public Y2 {
public:
    virtual void zMethod() {}
    virtual void xMethod() override {}
    virtual ~Z() {}
    
private:
    float zData;
}; 