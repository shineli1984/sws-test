import { configureStore } from '@reduxjs/toolkit';
import companiesReducer from '../features/companies/companiesSlice';

export default configureStore({
  reducer: {
    companies: companiesReducer,
  },
});
