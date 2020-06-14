import { createSlice } from '@reduxjs/toolkit';
import { equals } from 'ramda';
import axios from 'axios'

export const initialState = {
  params: null,
  exchanges: [
    "NasdaqGS",
    "ASX",
    "NYSE"
  ], // TODO: retriving exchanges from backend
  companies: [],
  sortBy: {
    desc: null,
    field: null
  },
  filters: {
    score: "",
    exchanges: []
  },
  loading: true,
  // TODO: this can be factored out to app level.
  errors: {
    loadingError: null
  }
  // TODO: pagination
}

export const companiesSlice = createSlice({
  name: 'companies',
  initialState,
  reducers: {
    sort: (state, action) => {
      const { field } = action.payload
      state.sortBy.field = field
    },
    sortDirection: (state, action) => {
      const { desc } = action.payload
      state.sortBy.desc = desc ? 1 : 0
    },
    filterScore: (state, action) => {
      const { score } = action.payload
      state.filters.score = score
    },
    filterExchanges: (state, action) => {
      const { values } = action.payload
      state.filters.exchanges = values || []
    },
    setLoading: (state, action) => {
      const { value } = action.payload
      state.loading = value
    },
    loadingCompanies: (state, action) => {
      const { params } = action.payload
      state.params = params
      state.loading = true
    },
    companiesLoaded: (state, action) => {
      const { companies, params } = action.payload

      // TODO: factor params comparing logic out.
      if (equals(state.params, params)) {
        state.companies = companies
        state.loading = false
        state.errors.loadingError = null
        state.params = null
      }
    },
    companiesLoadingFailed: (state, action) => {
      const { params } = action.payload

      if (equals(state.params, params)) {
        state.params = null
        state.loading = false
        state.errors.loadingError = "Loading companies failed, please try again later." 
      }
    }
  },
});

export const { sort, sortDirection, filterScore, filterExchanges, setLoading, loadingCompanies, companiesLoaded, companiesLoadingFailed } = companiesSlice.actions;

export const fetchCompanies = (filters, sortBy) => dispatch => {
  const baseUrl = "http://localhost:9998"
  const params = {
      desc: sortBy.desc,
      sortBy: sortBy.field,
      exchanges: filters.exchanges,
      score: filters.score || null,
  };
  dispatch(loadingCompanies({ params }))
  axios.get(baseUrl + "/companies", { params })
    .then(response => {
      dispatch(companiesLoaded({companies: response.data, params}))
    })
    .catch(_error => {
      dispatch(companiesLoadingFailed({ params }))
    })
}

export const selectFilters = state => state.companies.filters
export const selectSortBy = state => state.companies.sortBy
export const selectComapnies = state => state.companies.companies
export const selectLoading = state => state.companies.loading
export const selectExchanges = state => state.companies.exchanges
export const selectLoadingError = state => state.companies.errors.loadingError

export default companiesSlice.reducer;
