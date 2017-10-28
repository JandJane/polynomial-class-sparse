#include <algorithm>
#include <iostream>
#include <map>
#include <string>
#include <utility>
#include <vector>

template <typename Num>
class Polynomial {
public:
    template <class ForwardIter>
    Polynomial(ForwardIter first, ForwardIter last) {
        size_t i = 0;
        for (; first != last; ++first, ++i) {
            coef_[i] = *first;
        }
        normalize_coef();
    }

    Polynomial(const std::vector<Num>& coef)
        : Polynomial(coef.begin(), coef.end()) {}

    Polynomial(Num x = Num())
        : Polynomial(std::vector<Num>{x}) {}

    friend bool operator == (const Polynomial& that, const Polynomial& other) {
        return that.coef_ == other.coef_;
    }

    friend bool operator != (const Polynomial& that, const Polynomial& other) {
        return that.coef_ != other.coef_;
    }

    Num operator[] (size_t i) const {
        if (coef_.find(i) == coef_.cend()) {
            return Num();
        }
        return coef_.at(i);
    }

    int Degree() const {
        if (coef_.empty()) {
            return -1;
        }
        return coef_.crbegin()->first;
    }

    Polynomial& operator += (const Polynomial& other) {
        for (int i = 0; i <= other.Degree(); ++i) {
            this->coef_[i] += other[i];
        }
        normalize_coef();
        return *this;
    }

    friend Polynomial operator + (Polynomial that, const Polynomial& other) {
        return that += other;
    }

    Polynomial& operator -= (const Polynomial& other) {
        for (int i = 0; i <= other.Degree(); ++i) {
            this->coef_[i] -= other[i];
        }
        normalize_coef();
        return *this;
    }

    friend Polynomial operator - (Polynomial that, const Polynomial& other) {
        return that -= other;
    }

    friend Polynomial operator * (const Polynomial& that, const Polynomial& other) {
        Polynomial result;
        for (int j = 0; j <= other.Degree(); ++j) {
            for (int i = 0; i <= that.Degree(); ++i) {
                result.coef_[i + j] += that[i] * other[j];
            }
        }
        result.normalize_coef();
        return result;
    }

    Polynomial& operator *= (const Polynomial& other) {
        return *this = *this * other;
    }

    friend Polynomial operator & (const Polynomial& that, const Polynomial& other) {
        Polynomial result, other_pow = 1;
        int cur_pow = 0;
        for (const auto& cur : that.coef_) {
            while (cur_pow < cur.first) {
                other_pow *= other;
                ++cur_pow;
            }
            result += (cur.second) * other_pow;
        }
        result.normalize_coef();
        return result;
    }

    Num operator() (Num x) const {
        Num ans = 0;
        Num px = 1;
        for (int i = 0; i <= Degree(); ++i) {
            ans += px * (*this)[i];
            px *= x;
        }
        return ans;
    }

    typename std::map<size_t, Num>::const_iterator begin() const {
        return coef_.cbegin();
    }

    typename std::map<size_t, Num>::const_iterator end() const {
        return coef_.cend();
    }

    Polynomial& operator /= (const Polynomial& other) {
        return *this = divmod(other);
    }

    friend Polynomial operator / (Polynomial that, const Polynomial& other) {
        return that.divmod(other);
    }

    Polynomial& operator %= (const Polynomial& other) {
        divmod(other);
        return *this;
    }

    friend Polynomial operator % (Polynomial that, const Polynomial& other) {
        that.divmod(other);
        return that;
    }

    Polynomial operator , (Polynomial other) const {
        auto that = *this;
        while (other.Degree() != -1) {
            that.swap(other);
            other.divmod(that);
        }
        if (that.Degree() != -1) {
            Num c = (--that.end())->second;
            for (auto& x : that.coef_) {
                x.second /= c;
            }
        }
        return that;
    }

    template <typename T>
    friend std::ostream& operator <<(std::ostream&, const Polynomial<T>&);

 private:
    void normalize_coef() {
        auto p = coef_.begin();
        while (p != coef_.end()) {
            if (p->second == 0) {
                coef_.erase(p++);
            } else {
                ++p;
            }
        }
    }

    Polynomial divmod(const Polynomial& other) {
        Polynomial div;
        if (other.Degree() == -1) {
            return {};
        }
        while (this->Degree() >= other.Degree()) {
            int old_rem_deg = this->Degree();
            auto cur_coef = (--this->end())->second / (--other.end())->second;
            auto cur_deg = this->Degree() - other.Degree();
            Polynomial factor;
            factor.coef_[cur_deg] = cur_coef;
            div += factor;
            (*this) -= other * factor;
            if (this->Degree() == old_rem_deg) {
                this->coef_.erase(--this->coef_.end());
            }
        }
        this->normalize_coef();
        div.normalize_coef();
        return div;
    }

    void swap(Polynomial& other) {
        coef_.swap(other.coef_);
    }

    std::map<size_t, Num> coef_;
};

template <typename Num>
std::ostream& operator << (std::ostream& out, const Polynomial<Num>& p) {
    std::string plus_ = "";
    for (auto iter = p.coef_.crbegin(); iter != p.coef_.crend(); ++iter) {
        auto power = iter->first;
        auto cur = iter->second;
        if (cur > 0) {
            out << plus_;
        } else {
            out << '-';
            cur = -cur;
        }
        if (cur != 1) {
            out << cur;
            if (power > 0) {
                out << '*';
            }
        }
        if (cur == 1 && power == 0) {
            out << cur;
        }
        if (power > 0) {
            out << 'x';
            if (power > 1) {
                out << '^' << power;
            }
        }
        plus_ = "+";
    }
    if (p.Degree() == -1) {
        out << 0;
    }
    return out;
}