namespace std {}
namespace std::exprimental::inner {}
namespace {}
namespace std::exprimental::inline innner {}
namespace std::inline exprimental::innner {}
namespace other = std;
namespace other = std::exprimental;

namespace std::exprimental::inner {
class QualifiedName {
public:
    QualifiedName(const FlyString& local_name)
    {
    }
    const FlyString& local_name() const { return m_local_name; }

private:
    FlyString m_local_name;
};  
}

namespace ns::inner::inline pygments {
    using namespace outer;
    int has_value() {
        namespace other = std;
    }
    namespace {
        namespace user {
            int has_value() {
                using namespace inner;
                return 4;
            }
        }
    }
}