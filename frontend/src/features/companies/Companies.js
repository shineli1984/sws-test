import React, { useEffect } from 'react';
import { useSelector, useDispatch } from 'react-redux';
import {
  selectFilters,
  selectSortBy,
  fetchCompanies,
  selectComapnies
} from './companiesSlice';
import CompanyList from './CompanyList'
import Filters from './Filters'

export function Companies() {
  const filters = useSelector(selectFilters);
  const sortBy = useSelector(selectSortBy);
  const companies = useSelector(selectComapnies);
  const dispatch = useDispatch();
  useEffect(() => {
    dispatch(fetchCompanies(filters, sortBy))
  }, [filters, sortBy, dispatch])

  return <div>
    <Filters />
    <CompanyList companies={companies}/>
  </div>
}