import React from 'react'
import {
  filterScore,
  selectExchanges,
  filterExchanges,
  sort,
  sortDirection,
  selectFilters,
  selectSortBy,
  selectErrors
} from './companiesSlice';
import TextField from '@material-ui/core/TextField';
import { useSelector, useDispatch } from 'react-redux';
import Autocomplete from '@material-ui/lab/Autocomplete';
import Radio from '@material-ui/core/Radio';
import FormLabel from '@material-ui/core/FormLabel';
import FormControlLabel from '@material-ui/core/FormControlLabel';
import Switch from '@material-ui/core/Switch';
import Grid from '@material-ui/core/Grid';
import Box from '@material-ui/core/Box';
import Alert from '@material-ui/lab/Alert';

const Filters = () => {
  const exchanges = useSelector(selectExchanges);
  const filters = useSelector(selectFilters);
  const sortBy = useSelector(selectSortBy);
  const errors = useSelector(selectErrors);
  const dispatch = useDispatch();
  const dispatchSort = field => dispatch(sort({field}));
  const dispatchsortDirection = desc => dispatch(sortDirection({desc}));

  return <Box m={2}>
    <Grid container spacing={3}>
      {
        errors.length > 0 ? <Grid item xs={12}><Alert severity="error">{errors.join("&nbsp;")}</Alert></Grid> : null
      }
      <Grid item xs={12} sm={6} md={4}>
        <Autocomplete
          multiple
          disabled={exchanges.length === 0}
          value={filters.exchanges}
          filterSelectedOptions
          options={exchanges}
          getOptionLabel={option => option}
          onChange={(_e, values) => dispatch(
            filterExchanges({
              values
            })
          )}
          renderInput={(params) => (
            <TextField
              {...params}
              variant="standard"
              label="Exhanges"
              placeholder="Filter by exchanges"
            />
          )}
        />
      </Grid>
      <Grid item xs={12} sm={6}>
        <TextField 
          label="Above Score:&nbsp;"
          value={filters.score}
          type="number"
          onChange={e => dispatch(filterScore({score: e.target.value}))} />
      </Grid>
      <Grid item xs={6}>
        <FormLabel component="legend">Sort by</FormLabel>
        <FormControlLabel name="sortBy" checked={sortBy.field === "variance"} value="variance" control={<Radio />} label="Volatility" onChange={(_e, value) => dispatchSort("variance")} />
        <FormControlLabel name="sortBy" checked={sortBy.field === "score"} value="score" control={<Radio />} label="Score" onChange={(_e, value) => dispatchSort("score")} />
      
        <FormControlLabel
          control={
            <Switch
              disabled={sortBy.field === null}
              checked={sortBy.desc === 1}
              onChange={(_e, value) => dispatchsortDirection(value)}
              name="Descending"
              inputProps={{ 'aria-label': 'secondary checkbox' }}
            />
          }
          label="Descending"
        />
      </Grid>
    </Grid>
  </Box>
}
  

export default Filters