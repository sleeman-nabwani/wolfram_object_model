// Deep inheritance hierarchy
class Level1 {
public:
    virtual void level1Method() {}
    virtual ~Level1() {}
    
private:
    int level1Data;
};

class Level2 : public Level1 {
public:
    virtual void level2Method() {}
    virtual void level1Method() override {}
    virtual ~Level2() {}
    
private:
    double level2Data;
};

class Level3 : public Level2 {
public:
    virtual void level3Method() {}
    virtual void level2Method() override {}
    virtual ~Level3() {}
    
private:
    char level3Data;
};

class Level4 : public Level3 {
public:
    virtual void level4Method() {}
    virtual void level3Method() override {}
    virtual ~Level4() {}
    
private:
    float level4Data;
};

class Level5 : public Level4 {
public:
    virtual void level5Method() {}
    virtual void level4Method() override {}
    virtual ~Level5() {}
    
private:
    long level5Data;
}; 