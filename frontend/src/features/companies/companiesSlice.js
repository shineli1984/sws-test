import { createSlice } from '@reduxjs/toolkit';
import { compose, equals, values, reject, isNil } from 'ramda';
import axios from 'axios'
import { companiesUrl, exchangesUrl } from '../../config'

export const initialState = {
  params: null,
  exchanges: [],
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
  errors: {
    companiesLoadingError: null,
    exchangesLoadingError: null
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
      const { data, params } = action.payload

      if (paramsMatch(state, params)) {
        state.companies = data.companies
        state.loading = false
        state.errors.companiesLoadingError = null
        state.params = null
      }
    },
    companiesLoadingFailed: (state, action) => {
      const { params } = action.payload

      if (paramsMatch(state, params)) {
        state.params = null
        state.loading = false
        state.errors.companiesLoadingError = "Loading companies failed, please try again later." 
      }
    },
    exchangesLoaded: (state, action) => {
      const { data } = action.payload
      state.exchanges = data
    },
    exchangesLoadingFailed: (state) => {
      state.errors.exchangesLoadingError = "Loading exchanges failed, please try again later." 
    }
  },
});

const paramsMatch = (state, params) => equals(state.params, params)

export const { 
  sort, 
  sortDirection, 
  filterScore, 
  filterExchanges, 
  setLoading, 
  loadingCompanies, 
  companiesLoaded, 
  companiesLoadingFailed,
  loadingExchanges,
  exchangesLoaded,
  exchangesLoadingFailed
} = companiesSlice.actions;

export const fetchExchanges = dispatch => {
  axios.get(exchangesUrl)
    .then(response => {
      dispatch(exchangesLoaded({data: response.data}))
    })
    .catch(_error => {
      dispatch(exchangesLoadingFailed())
    })
}

export const fetchCompanies = (filters, sortBy) => dispatch => {
  const params = {
      desc: sortBy.desc,
      sortBy: sortBy.field,
      exchanges: filters.exchanges,
      score: filters.score || null,
  };
  dispatch(loadingCompanies({ params }))
  axios.get(companiesUrl, { params })
    .then(response => {
      dispatch(companiesLoaded({data: response.data, params}))
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
export const selectErrors = state => compose(reject(isNil), values)(state.companies.errors)

export default companiesSlice.reducer;
