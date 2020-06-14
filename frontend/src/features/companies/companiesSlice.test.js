import companiesSliceReducer, { initialState, companiesSlice, sort, sortDirection, filterScore, filterExchanges, setLoading, loadingCompanies, companiesLoaded, companiesLoadingFailed } from './companiesSlice';

describe("companiesSlice", () => {
  describe("reducer", () => {
    test("sort action", () => {
      const field = "field"
      const action = sort({
        field
      })
      expect(companiesSliceReducer(initialState, action).sortBy.field).toBe(field)
    })

    describe("sortDirection action", () => {
      test("set desc to true", () => {
        const action = sortDirection({desc : true})
        expect(companiesSliceReducer(initialState, action).sortBy.desc).toBe(1)
      })

      test("set desc to false", () => {
        const action = sortDirection({desc : false})
        expect(companiesSliceReducer(initialState, action).sortBy.desc).toBe(0)
      })
    })

    test("set filterScore", () => {
      const score = 2
      const action = filterScore({
        score
      })
      expect(companiesSliceReducer(initialState, action).filters.score).toBe(score)
    })
    
    test("filterExchanges", () => {
      const exchanges = ["NYSE", "ASX"]
      const action = filterExchanges({
        values: exchanges
      })
      expect(companiesSliceReducer(initialState, action).filters.exchanges).toBe(exchanges)
    })

    test("setLoading", () => {
      const loading = !initialState.loading
      const action = setLoading({
        value: loading
      })
      expect(companiesSliceReducer(initialState, action).loading).toBe(loading)
    })

    test("loadingCompanies", () => {
      const params = {}
      const action = loadingCompanies({
        params
      })
      const actual = companiesSliceReducer(initialState, action)
      expect(actual.params).toEqual(params)
      expect(actual.loading).toBe(true)
    })

    describe("companiesLoaded", () => {
      test("latest companiesLoaded", () => {
        const params = {}
        const loadCompaniesAction = loadingCompanies({
          params
        })
        const companies = []
        const action = companiesLoaded({
          companies,
          params
        })
        const companiesLoadedState = companiesSliceReducer(initialState, loadCompaniesAction)
        const actual = companiesSliceReducer(companiesLoadedState, action)
        expect(actual.params).toBe(null)
        expect(actual.errors.loadingError).toBe(null)
        expect(actual.loading).toBe(false)
        expect(actual.companies).toBe(companies)
      })

      test("lagged companiesLoaded", () => {
        const params = {}
        const loadCompaniesAction = loadingCompanies({
          params
        })
        const companies = []
        const oldParams = {}
        const action = companiesLoaded({
          companies,
          oldParams
        })
        const companiesLoadedState = companiesSliceReducer(initialState, loadCompaniesAction)
        const actual = companiesSliceReducer(companiesLoadedState, action)
        expect(actual.params).toBe(params)
        expect(actual.errors.loadingError).toBe(null)
        expect(actual.loading).toBe(true)
        expect(actual.companies).toBe(initialState.companies)
      })
    })

    test("companiesLoadingFailed", () => {
      const action = companiesLoadingFailed({params: null})
      const actual = companiesSliceReducer(initialState, action)
      expect(actual.params).toBe(null)
      expect(actual.loading).toBe(false)
      expect(actual.errors.loadingError).toBe("Loading companies failed, please try again later.")
    })
  });
})