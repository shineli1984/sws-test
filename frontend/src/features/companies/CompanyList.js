import React from 'react';
import Table from '@material-ui/core/Table';
import TableBody from '@material-ui/core/TableBody';
import TableCell from '@material-ui/core/TableCell';
import TableContainer from '@material-ui/core/TableContainer';
import TableHead from '@material-ui/core/TableHead';
import TableRow from '@material-ui/core/TableRow';
import Paper from '@material-ui/core/Paper';


const CompanyList = ({ companies }) => (
  <TableContainer component={Paper}>
    <Table>
        <TableHead>
          <TableRow>
            <TableCell>Comapny Name</TableCell>
            <TableCell>Ticker</TableCell>
            <TableCell align="right">Last Close</TableCell>
            <TableCell align="right">Score</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {companies.map(({ company, close, score }) => (
            <TableRow key={company.entityKey}>
              <TableCell component="th" scope="row">
                {company.entityVal.swsCompanyName}
              </TableCell>
              <TableCell>{company.entityVal.swsCompanyUnique_symbol}</TableCell>
              <TableCell align="right">{close}</TableCell>
              <TableCell align="right">{score}</TableCell>
            </TableRow>
          ))}
        </TableBody>
      </Table>
  </TableContainer>
)

export default CompanyList