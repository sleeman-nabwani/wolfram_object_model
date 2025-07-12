// Mixed inheritance with multiple virtual and non-virtual bases
class VirtualBase1 {
public:
    virtual void vb1Method() {}
    virtual ~VirtualBase1() {}
    
private:
    int vb1Data;
};

class VirtualBase2 {
public:
    virtual void vb2Method() {}
    virtual ~VirtualBase2() {}
    
private:
    double vb2Data;
};

class NonVirtualBase1 {
public:
    virtual void nvb1Method() {}
    virtual ~NonVirtualBase1() {}
    
private:
    char nvb1Data;
};

class NonVirtualBase2 {
public:
    virtual void nvb2Method() {}
    virtual ~NonVirtualBase2() {}
    
private:
    float nvb2Data;
};

class MixedDerived1 : virtual public VirtualBase1, public NonVirtualBase1 {
public:
    virtual void md1Method() {}
    virtual void vb1Method() override {}
    virtual void nvb1Method() override {}
    virtual ~MixedDerived1() {}
    
private:
    long md1Data;
};

class MixedDerived2 : virtual public VirtualBase2, public NonVirtualBase2 {
public:
    virtual void md2Method() {}
    virtual void vb2Method() override {}
    virtual void nvb2Method() override {}
    virtual ~MixedDerived2() {}
    
private:
    short md2Data;
};

class FinalMixed : public MixedDerived1, public MixedDerived2 {
public:
    virtual void finalMethod() {}
    virtual void vb1Method() override {}
    virtual void vb2Method() override {}
    virtual ~FinalMixed() {}
    
private:
    int finalData;
}; 